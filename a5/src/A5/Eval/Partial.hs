{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- HLINT ignore "Avoid lambda" -}

-- | Partial evaluation.
module A5.Eval.Partial
  ( Type (..),
    GluedVal (..),
    GluedArray (..),
    DynamicInt (..),
    DynamicBool (..),
    GluedProg (..),
    staticInt,
    staticBool,
    staticArray,
    dynamic,
    residualProg,
    residualVal,
  )
where

import A5.Data.Array.Pure (Array)
import A5.Data.Array.Pure qualified as Array
import A5.Language
import A5.Poly.Sparse (Poly)
import A5.Poly.Sparse qualified as Poly
import Control.Monad
import Data.Semigroup
import Prelude hiding (and, not, or)

--------------------------------------------------------------------------------
-- Types

data Type
  = TpInt
  | TpBool
  | TpArray
  | TpUnknown
  deriving (Show, Ord, Eq)

prettyType :: Type -> String
prettyType TpInt = "int"
prettyType TpBool = "bool"
prettyType TpArray = "array"
prettyType TpUnknown = "dynamic"

--------------------------------------------------------------------------------
-- Values

-- | Values.
data GluedVal var
  = -- | Integers.
    GInt (GluedInt var)
  | -- | Boolean.
    GBool (GluedBool var)
  | -- | Arrays.
    GArray (GluedArray var)
  | -- | Things of dynamic type.
    GUnknown var
  deriving (Show, Ord, Eq)

type GluedInt var = Poly Int (Sum Word) (DynamicInt var)

-- | Dynamic integers.
data DynamicInt var
  = -- | A variable.
    IVar var
  | -- | The length of a dynamic array.
    ILength var
  deriving (Show, Eq, Ord)

type GluedBool var = Poly Bool Any (DynamicBool var)

-- | Dynamic booleans.
data DynamicBool var
  = BVar var
  | BLeq (GluedInt var) (GluedInt var)
  | BLt (GluedInt var) (GluedInt var)
  deriving (Show, Ord, Eq)

data GluedArray var
  = -- | A fully static array.
    StaticArray (Array (GluedVal var))
  | -- | A dynamic array.
    DynamicArray var
  deriving (Show, Ord, Eq)

dynamic :: (Ord var) => var -> Type -> GluedVal var
dynamic var TpInt = GInt (Poly.id (IVar var))
dynamic var TpBool = GBool (Poly.id (BVar var))
dynamic var TpArray = GArray (DynamicArray var)
dynamic var TpUnknown = GUnknown var

staticInt :: Int -> GluedVal var
staticInt i = GInt (Poly.const i)

staticBool :: Bool -> GluedVal var
staticBool b = GBool (Poly.const b)

staticArray :: Array (GluedVal var) -> GluedVal var
staticArray = GArray . StaticArray

valType :: GluedVal var -> Type
valType (GInt _) = TpInt
valType (GBool _) = TpBool
valType (GArray _) = TpArray
valType (GUnknown _) = TpUnknown

--------------------------------------------------------------------------------
-- Computations

data GluedStep var
  = GGetDynamicArray (GluedArray var) (GluedInt var)
  | GSetDynamicArray (GluedArray var) (GluedInt var) (GluedVal var)
  | GForUp (GluedInt var) (GluedInt var) (GluedVal var) (var -> var -> GluedProg var (GluedVal var))
  | GForDown (GluedInt var) (GluedInt var) (GluedVal var) (var -> var -> GluedProg var (GluedVal var))

data GluedProg var a
  = GRet a
  | GCrash String
  | GLet (GluedStep var) (var -> GluedProg var a)
  | GIf (GluedBool var) (GluedProg var a) (GluedProg var a)
  deriving (Functor)

instance Applicative (GluedProg var) where
  pure = GRet
  (<*>) = ap

instance Monad (GluedProg var) where
  return = pure
  (GRet a) >>= f = f a
  GCrash msg >>= _ = GCrash msg
  GLet step prog >>= f = GLet step (prog >=> f)
  GIf b prog1 prog2 >>= f = GIf b (prog1 >>= f) (prog2 >>= f)

instance MonadFail (GluedProg var) where
  fail = GCrash

--------------------------------------------------------------------------------
-- Checks

unwrapBool :: (Ord var) => String -> GluedVal var -> GluedProg var (GluedBool var)
unwrapBool _loc (GBool b) = pure b
unwrapBool _loc (GUnknown var) = pure (Poly.id (BVar var))
unwrapBool loc v =
  fail $
    unlines
      [ "type error.",
        "Expected a bool in the " <> loc,
        "but got something of type " <> prettyType (valType v)
      ]

unwrapInt :: (Ord var) => String -> GluedVal var -> GluedProg var (GluedInt var)
unwrapInt _loc (GInt n) = pure n
unwrapInt _loc (GUnknown var) = pure (Poly.id (IVar var))
unwrapInt loc v =
  fail $
    unlines
      [ "type error.",
        "Expected a bool in the " <> loc,
        "but got something of type " <> prettyType (valType v)
      ]

unwrapArray :: String -> GluedVal var -> GluedProg var (GluedArray var)
unwrapArray _loc (GArray arr) = pure arr
unwrapArray loc v =
  fail $
    unlines
      [ "Type error:",
        "Expected an array in the " <> loc,
        "but got something of type " <> prettyType (valType v)
      ]

intBinOp :: (Ord var) => String -> (GluedInt var -> GluedInt var -> GluedInt var) -> GluedVal var -> GluedVal var -> GluedProg var (GluedVal var)
intBinOp name f x y = do
  vx <- unwrapInt ("first argument of " <> name) x
  vy <- unwrapInt ("second argument of " <> name) y
  pure (GInt (f vx vy))

intCmpOp :: (Ord var) => String -> (GluedInt var -> GluedInt var -> GluedBool var) -> GluedVal var -> GluedVal var -> GluedProg var (GluedVal var)
intCmpOp name f x y = do
  vx <- unwrapInt ("first argument of " <> name) x
  vy <- unwrapInt ("second argument of " <> name) y
  pure (GBool (f vx vy))

boolUnOp :: (Ord var) => String -> (GluedBool var -> GluedBool var) -> GluedVal var -> GluedProg var (GluedVal var)
boolUnOp name f x = do
  vx <- unwrapBool ("first argument of " <> name) x
  pure (GBool (f vx))

boolBinOp :: (Ord var) => String -> (GluedBool var -> GluedBool var -> GluedBool var) -> GluedVal var -> GluedVal var -> GluedProg var (GluedVal var)
boolBinOp name f x y = do
  vx <- unwrapBool ("first argument of " <> name) x
  vy <- unwrapBool ("second argument of " <> name) y
  pure (GBool (f vx vy))

--------------------------------------------------------------------------------
-- Interpretation

instance (Ord var) => Lang (GluedVal var) (GluedProg var (GluedVal var)) where
  int = staticInt
  add = intBinOp "add" (Poly.+)
  sub = intBinOp "sub" (Poly.-)
  mul = intBinOp "mul" (Poly.*)

  bool = staticBool
  xor = intBinOp "xor" (Poly.+)
  and = intBinOp "and" (Poly.*)
  or = boolBinOp "or" (\x y -> x Poly.+ y Poly.+ x Poly.* y)
  not = boolUnOp "not" (\x -> Poly.const True Poly.+ x)

  -- for these guys we check if the two operands are constants then just do
  -- the opertion and return the output.
  leq =
    intCmpOp
      "leq"
      ( \a b ->
          case (Poly.viewConst a, Poly.viewConst b) of
            -- two consts
            (Just ia, Just ib) -> Poly.const (ia <= ib)
            -- now its a BLeq function
            _ -> Poly.id (BLeq a b)
      )

  -- same but other operator
  lt =
    intCmpOp
      "lt"
      ( \a b ->
          case (Poly.viewConst a, Poly.viewConst b) of
            (Just ia, Just ib) -> Poly.const (ia < ib)
            _ -> Poly.id (BLt a b)
      )

  array vs = staticArray (Array.fromList vs)

  getArray arr i = do
    -- first we need to get the array and the index
    arry <- unwrapArray "first arg of get arr" arr
    index <- unwrapInt "second arg of get arr" i
    case (arry, Poly.viewConst index) of
      -- static so we know at run time and extract value
      (StaticArray a, Just idx) ->
        case Array.get idx a of
          Just v -> pure v
          Nothing -> fail "index out of bounds"
      -- tell them we need them to find the item. then after we find it the we give
      --
      _ -> GLet (GGetDynamicArray arry index) (\item -> pure $ GUnknown item)

  setArray arr i v = do
    -- first we need to get the array and the index
    arry <- unwrapArray "first arg of get arr" arr
    index <- unwrapInt "second arg of get arr" i
    case (arry, Poly.viewConst index) of
      -- static so we know at run time and extract value
      (StaticArray a, Just idx) ->
        case Array.set idx v a of
          Just a' -> pure (staticArray a')
          Nothing -> fail "index out of bounds"
      -- tell them we need them to set the item. and we know it will be a dynamic array
      -- since when you set synamic arrays you get a synamic arry back
      _ -> GLet (GSetDynamicArray arry index v) (\a' -> pure $ GArray $ DynamicArray a')

  lenArray arr = do
    a <- unwrapArray "lenArray array" arr
    case a of
      -- on static arrays we know the size so just get it and return it
      StaticArray arry -> pure $ GInt $ Poly.const $ length arry
      DynamicArray arry -> pure $ GInt $ Poly.id $ ILength arry

  let_ c body = do
    -- take out the GludeVal undeneath and toss into to the body
    c' <- c
    body c'

  ret = pure

  forUp lo hi acc body = do
    l <- unwrapInt "low of forup" lo
    h <- unwrapInt "low of forup" hi
    case (Poly.viewConst l, Poly.viewConst h) of
      -- check if we know they are constants
      -- if they are we know the size and can expand the forloop
      (Just l', Just h') ->
        -- atp we know how many times to run body so we just run it that many tiems.
        -- the body requires a loop counter so we create a list of numbers and pass it.
        -- the body will return something which we can mappend, so we foldm returning the output the
        -- bodies
        -- maybe this is too good? idk its fialing the test for me...
        -- foldM (\a i -> body (staticInt i) a) acc [l' .. h' - 1]
        -- anyways so i end up doing the above thing but explicitly. i guess
        -- the bind is not doing what i think its doing and shrinking some values? weird!
        build acc l'
        where
          build a i =
            if i >= h'
              then
                pure a
              else do
                a' <- body (staticInt i) a
                build a' (i + 1)
      -- we dont know how big the loop is so we tell them
      -- the loop body has some acc which we will know after called r
      -- the acc can be of any type so we kinda loose that info by weapping
      -- in a gunkonwn but thats the best we can do :(
      -- the loop var is and int so we keep that info
      _ -> GLet (GForUp l h acc (\l' acc' -> body (GInt $ Poly.id $ IVar l') (GUnknown acc'))) (\r -> pure (GUnknown r))

  forDown hi lo acc body = do
    -- copy the above code but in inverse direction do the fold
    l <- unwrapInt "low of fordown" lo
    h <- unwrapInt "low of fordown" hi
    case (Poly.viewConst l, Poly.viewConst h) of
      (Just l', Just h') ->
        build acc h'
        where
          build a i =
            if i < l'
              then
                pure a
              else do
                a' <- body (staticInt i) a
                build a' (i - 1)

      -- foldM (\a i -> body (staticInt i) a) acc [h' .. l']
      _ -> GLet (GForUp l h acc (\h' acc' -> body (GInt $ Poly.id $ IVar h') (GUnknown acc'))) (\r -> pure (GUnknown r))

  if_ b thb elb = do
    -- if the bool is a const just do that branch
    b' <- unwrapBool "condition of if" b
    case Poly.viewConst b' of
      Just True -> thb
      Just False -> elb
      Nothing -> GIf b' thb elb

  crash = GCrash

--------------------------------------------------------------------------------
-- Residualizing
--
-- You do not need to touch this code.

residualProg :: (Lang val cmp) => GluedProg val (GluedVal val) -> cmp
residualProg (GRet val) = residualVal val ret
residualProg (GCrash msg) = crash msg
residualProg (GLet neu body) = let_ (residualStep neu) (residualProg . body)
residualProg (GIf b prog1 prog2) =
  residualBool b \vb ->
    if_ vb (residualProg prog1) (residualProg prog2)

residualStep :: (Lang val cmp) => GluedStep val -> cmp
residualStep (GGetDynamicArray arr i) =
  residualArray arr \rarr ->
    residualInt i \ri ->
      getArray rarr ri
residualStep (GSetDynamicArray arr i v) =
  residualArray arr \rarr ->
    residualInt i \ri ->
      residualVal v \rv ->
        setArray rarr ri rv
residualStep (GForUp lo hi acc body) =
  residualInt lo \rlo ->
    residualInt hi \rhi ->
      residualVal acc \racc ->
        forUp rlo rhi racc \varinit varacc -> residualProg (body varinit varacc)
residualStep (GForDown hi lo acc body) =
  residualInt hi \rhi ->
    residualInt lo \rlo ->
      residualVal acc \racc ->
        forDown rhi rlo racc \varinit varacc -> residualProg (body varinit varacc)

residualVal :: (Lang val cmp) => GluedVal val -> (val -> cmp) -> cmp
residualVal (GInt n) = residualInt n
residualVal (GBool b) = residualBool b
residualVal (GArray arr) = residualArray arr
residualVal (GUnknown v) = \k -> k v

residualInt :: (Lang val cmp) => Poly Int (Sum Word) (DynamicInt val) -> (val -> cmp) -> cmp
residualInt p k =
  case Poly.viewConst p of
    Just i -> k (int i)
    Nothing ->
      runArith $ Poly.eval p (Arith . ret . int) (\c n -> c Poly.^ getSum n) \case
        IVar x -> Arith (k x)
        ILength x -> Arith (let_ (lenArray x) k)

residualBool :: (Lang val cmp) => Poly Bool Any (DynamicBool val) -> (val -> cmp) -> cmp
residualBool p k =
  case Poly.viewConst p of
    Just b -> k (bool b)
    Nothing ->
      runLogic $ Poly.eval p (Logic . ret . bool) (\c b -> if getAny b then c else Logic (ret $ bool True)) \case
        BVar x -> Logic (k x)
        BLeq x y -> Logic $
          residualInt x \rx ->
            residualInt y \ry ->
              let_ (leq rx ry) k
        BLt x y -> Logic $
          residualInt x \rx ->
            residualInt y \ry ->
              let_ (lt rx ry) k

residualArray :: (Lang val cmp) => GluedArray val -> (val -> cmp) -> cmp
residualArray (DynamicArray val) k = k val
residualArray (StaticArray vs) k =
  foldl (\k' v rvs -> residualVal v \rv -> k' (rv : rvs)) (k . array) vs []
