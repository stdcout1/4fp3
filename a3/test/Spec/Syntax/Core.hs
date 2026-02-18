-- | Tests for 'A3.Syntax.Core'
module Spec.Syntax.Core
  (
   lawfulRenameSpec
  , coreSyntaxSpec
  -- $generators
  , genNameString
  , genNameDisjoint
  , genName
  , genBinderDisjoint
  , genBinder
  , genType
  , genTermDisjoint
  , genTerm
  ) where

import A3.Syntax.Core

import Data.Set (Set)
import Data.Set qualified as Set

import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Spec.Permutation

import Test.Tasty
import Test.Tasty.Hedgehog

-- | A specification of the @freshen@ function.
freshenSpec :: TestTree
freshenSpec =
  testGroup "freshen"
  [ testProperty "freshen disjoint" $ H.property do
      x <- H.forAll (genNameString (Range.exponential 0 10))
      nms <- H.forAll (Gen.set (Range.linear 0 100) genName)
      H.assert (Set.notMember (freshen x nms) nms)
  ]

-- | A specification for the laws of the 'Rename' class.
lawfulRenameSpec :: (Show a, Eq a, Rename a) => H.Gen a -> [TestTree]
lawfulRenameSpec gen =
  [ testProperty "rename identity" $ H.property do
      a <- H.forAll gen
      rename a mempty H.=== a
  , testProperty "rename compose" $ H.property do
      a <- H.forAll gen
      p1 <- H.forAll (genPermutation genName)
      p2 <- H.forAll (genPermutation genName)
      rename a (p1 <> p2) H.=== rename (rename a p1) p2
  , testProperty "rename freevars" $ H.property do
      a <- H.forAll gen
      -- Make sure that the permutaton permutes a set of names that is disjoint
      -- from the free vars of @aa@.
      p <- H.forAll (genPermutation (genNameDisjoint (freeVars a)))
      rename a p H.=== a
  ]

-- | A specification that a function @f :: a -> b@ commmutes with renaming.
equivariantSpec :: (Show a, Eq a, Rename a, Show b, Eq b, Rename b) => String -> (a -> b) -> H.Gen a -> TestTree
equivariantSpec nm f gen =
  testProperty nm $ H.property do
    a <- H.forAll gen
    p <- H.forAll (genPermutation genName)
    f (rename a p) H.=== rename (f a) p

coreSyntaxSpec :: TestTree
coreSyntaxSpec =
  testGroup "Syntax.Core"
  [ -- Basic laws that every instance must uphold.
    freshenSpec
  , testGroup "Rename instances"
    [ testGroup "Unit" $ lawfulRenameSpec (pure ())
    , testGroup "(Name, Name)" $ lawfulRenameSpec ((,) <$> genName <*> genName)
    , testGroup "Either Name Name" $ lawfulRenameSpec (Gen.choice [ Left <$> genName, Right <$> genName])
    , testGroup "[Name]" $ lawfulRenameSpec (Gen.list (Range.linear 0 10) genName)
    , testGroup "Name" $ lawfulRenameSpec genName
    , testGroup "Binder" $ lawfulRenameSpec genName
    , testGroup "Term" $ lawfulRenameSpec genTerm
    ]
    -- Equivariance tests. These tell us how operations ought to interact with renaming,
    -- and completely force our hand when it comes to how we implement @Renaming@ instances.
    , testGroup "Equivariance"
    [ testGroup "Pairs"
      [ equivariantSpec "fst" fst ((,) <$> genName <*> genName)
      , equivariantSpec "snd" snd ((,) <$> genName <*> genName)
      ]
    , testGroup "Either"
      [ equivariantSpec "Left" (Left :: Name -> Either Name Name) genName
      , equivariantSpec "Right" (Right :: Name -> Either Name Name) genName
      ]
    , testGroup "List"
      [ equivariantSpec "Nil" (const ([] :: [Name])) (pure ())
      , equivariantSpec "Cons" (uncurry (:)) ((,) <$> genName <*> Gen.list (Range.linear 0 10) genName)
      ]
    , testGroup "Term"
      [ equivariantSpec "Var" Var genName
      , equivariantSpec "Lam" Lam (genBinder genType genTerm)
      , equivariantSpec "App" (uncurry App) ((,) <$> genTerm <*> genTerm)
      , equivariantSpec "Top" (const Top) (pure ())
      , equivariantSpec "Bot" (const Bot) (pure ())
      , equivariantSpec "And" (uncurry And) ((,) <$> genTerm <*> genTerm)
      , equivariantSpec "Or" (uncurry Or) ((,) <$> genTerm <*> genTerm)
      , equivariantSpec "Implies" (uncurry Implies) ((,) <$> genTerm <*> genTerm)
      , equivariantSpec "Exists" Exists (genBinder genType genTerm)
      , equivariantSpec "ForAll" ForAll (genBinder genType genTerm)
      , equivariantSpec "Eq" (\(tm1, tm2, DontRename tp) -> Eq tm1 tm2 tp) ((,,) <$> genTerm <*> genTerm <*> (DontRename <$> genType))
      ]
    ]
  ]

-- * Generators

-- | Generate a name-like string, with a bias towards the start
-- of the alphabet.
genNameString :: forall m. (H.MonadGen m) => H.Range Int -> m String
genNameString range = do
  c <- genNameChar
  cs <- Gen.string range genNameChar
  pure (c:cs)
  where
    -- Biased generator for characters, which it more likely to find name conflict problems.
    genNameChar :: m Char
    genNameChar = Gen.frequency
      [ (1024, Gen.constant 'a')
      , (256, Gen.enum 'b' 'f')
      , (16, Gen.enum 'g' 'l')
      , (4, Gen.enum 'm' 'r')
      , (1, Gen.enum 's' 'z')
      ]

-- | Generate a 'Name' while avoiding a set of names.
genNameDisjoint :: forall m. (H.MonadGen m) => Set Name -> m Name
genNameDisjoint avoid = do
  str <- genNameString (Range.exponential 0 4)
  pure $ freshen str avoid

genName :: forall m. (H.MonadGen m) => m Name
genName = genNameDisjoint Set.empty

-- | Generate a 'Binder' while avoiding a set of names.
genBinderDisjoint :: forall m ann a. (H.MonadGen m) => Set Name -> m ann -> m a -> m (Binder ann a)
genBinderDisjoint avoid genAnn gen = Binder <$> genNameDisjoint avoid <*> genAnn <*> gen

genBinder :: forall m ann a. (H.MonadGen m) => m ann -> m a -> m (Binder ann a)
genBinder = genBinderDisjoint Set.empty

-- | Generate a 'Type'.
genType :: forall m. (H.MonadGen m) => m Type
genType =
  Gen.recursive Gen.choice
  [ pure Prop
  ]
  [ Gen.subterm2 genType genType Fn
  ]

-- | Generate a 'Term' while avoiding a set of names.
genTermDisjoint :: forall m. (H.MonadGen m) => Set Name -> m Term
genTermDisjoint avoid =
  Gen.recursive Gen.choice
  [ Var <$> genNameDisjoint avoid
  , pure Top
  , pure Bot
  ]
  [ subBinder (genTermDisjoint avoid) Lam
  , Gen.subterm2 (genTermDisjoint avoid) (genTermDisjoint avoid) App
  , Gen.subtermM2 (genTermDisjoint avoid) (genTermDisjoint avoid) \tm1 tm2 -> Eq tm1 tm2 <$> genType
  , Gen.subterm2 (genTermDisjoint avoid) (genTermDisjoint avoid) And
  , Gen.subterm2 (genTermDisjoint avoid) (genTermDisjoint avoid) Or
  , Gen.subterm2 (genTermDisjoint avoid) (genTermDisjoint avoid) Implies
  , subBinder (genTermDisjoint avoid) ForAll
  , subBinder (genTermDisjoint avoid) Exists
  ]
  where
    subBinder :: m Term -> (Binder Type Term -> Term) -> m Term
    subBinder gen k = Gen.subtermM gen \tm -> k <$> genBinderDisjoint avoid genType (pure tm)

genTerm :: forall m. (H.MonadGen m) => m Term
genTerm = genTermDisjoint Set.empty
