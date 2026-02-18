-- | The kernel of the proof assistant.
module A3.Kernel
  (
  -- $typecheck
    Check, runCheck
  , Infer, runInfer
  , KernelError(..)
  -- $structural
  , var, chk, ann
  -- $functions
  , lam , app
  -- $propositions
  , top, bot, and, or, implies, exists, forAll, eq
  -- $proofs
  , Backward, runBackward
  , Forward, runForward
  , assumption, hint, forward
  -- $true
  , topIntro
  -- $false
  , botElim
  -- $and
  , andIntro, andElimLeft, andElimRight
  -- $or
  , orIntroLeft, orIntroRight, orElim
  -- $implies
  , impliesIntro, impliesElim
  -- $exists
  , existsIntro, existsElim
  -- $forall
  , forAllIntro, forAllElim
  -- $equality
  , refl, leibniz, funext, beta, cong, propext
  -- $tacticals
  , inspect, sorry
  ) where

import Prelude hiding (and, or)

import A3.Permutation qualified as P

import A3.Pretty (Pretty(..))
import A3.Pretty qualified as Pretty

import A3.Syntax.Context (Context)
import A3.Syntax.Context qualified as Ctx
import A3.Syntax.Substitution (Substitute(..))

import A3.Syntax.Core

data KernelError
  = VariableNotInScope (Context Type) Name
  -- ^ A term variable is not in scope.
  | AssumptionNotInScope (Context Type) (Context Term) Name
  -- ^ An assumption is not in scope.
  | CheckMismatch String (Context Type) Type
  -- ^ A @CheckMismatch ruleName ctx tp@ signals that the
  -- check rule @ruleName@ running in context @ctx@ does not
  -- apply to the type @tp@.
  | InferMismatch String (Context Type) String Term Type
  -- ^ An @InferMismatch ruleName ctx expected tm actual@ signals that the
  -- infer rule @ruleName@ running in context @ctx@ expected something that looks
  -- like @expected@, but got a term @tm@ that has type @actual@.
  | TypeMismatch (Context Type) Type Term Type
  -- ^ A @TypeMismatch ctx expected tm actual@ signals that we needed something of
  -- type @expected@, but the term @tm@ has type @actual@.
  | IntroMismatch String (Context Type) (Context Term) Term
  -- ^ An @IntroMismatch ruleName ctx hyps goal@ signals that the
  -- forward rule @ruleName@ running in context @ctx@ with assumptions @hyps@ does not
  -- apply to the goal @goal@.
  | ElimMismatch String (Context Type) (Context Term) String Term
  -- ^ An @ElimMismatch ruleName ctx hyps expected actual@ signals that the
  -- elim rule @ruleName@ running in context @ctx@ with assumptions @hyps@ expected something
  -- that looks like @expected@, but got a goal that looks like @actual@.
  | GoalMismatch (Context Type) (Context Term) Term Term
  -- ^ A @GoalMismatch ctx hyps expected actual@ signals that we were trying to prove
  -- the goal @expected@, but actually proved @actual@.
  | TermMismatch (Context Type) (Context Term) Term Term Type
  -- ^ A @TermMismatch ctx tm1 tm2 tp@ signals that we expected two terms @tm1@
  -- and @tm2@ of type @tp@ to be equal.
  | Sorry String (Context Type) (Context Term) Term
  -- ^ Encountered a @sorry msg@.

-- $typecheck
-- * Typechecking

-- | A checking rule @Γ ⊢ e : α chk@
newtype Check = Check { runCheck :: Context Type -> Type -> Either KernelError Term }

-- | An inference rule @Γ ⊢ e : α syn@
newtype Infer = Infer { runInfer :: Context Type -> Either KernelError (Term, Type) }

-- $structural
-- ** Structural rules

var :: Name -> Infer
var x = Infer \ctx ->
  case Ctx.lookup x ctx of
    Just tp -> pure (Var x, tp)
    Nothing -> Left (VariableNotInScope ctx x)

chk :: Infer -> Check
chk = _chk

ann :: Check -> Type -> Infer
ann = _ann

-- $functions
-- ** Functions

lam :: String -> (Name -> Check) -> Check
lam x bodyRule = Check \ctx goal ->
  case goal of
    Fn dom cod -> do
      -- Make sure that we have a fresh name here!
      let (nm, ctx') = Ctx.extend ctx x dom
      tm <- runCheck (bodyRule nm) ctx' cod
      pure (Lam (Binder nm dom tm))
    _ -> Left (CheckMismatch "lam" ctx goal)

app :: Infer -> Check -> Infer
app fnRule argRule = Infer \ctx -> do
  (fn, tp) <- runInfer fnRule ctx
  case tp of
    Fn dom cod -> do
      arg <- runCheck argRule ctx dom
      pure (App fn arg, cod)
    _ -> Left (InferMismatch "app" ctx "function" fn tp)

-- $propositions
-- ** Propositions

top :: Check
top = _top

bot :: Check
bot = _bot

and :: Check -> Check -> Check
and = _and

or :: Check -> Check -> Check
or = _or

implies :: Check -> Check -> Check
implies = _implies

exists :: String -> Type -> (Name -> Check) -> Check
exists = _exists

forAll :: String -> Type -> (Name -> Check) -> Check
forAll = _forAll

eq :: Infer -> Check -> Check
eq = _eq

-- $theorem
-- * Theorems

-- | Backwards proofs.
--
-- Note that only @runBackward@ is exported, and not the @Backward@
-- constructor. This enables us to know that any 'Backward' created outside
-- this module must constitute a valid proof, as it must have been formed
-- by piecing together trusted rules.
newtype Backward = Backward { runBackward :: Context Type -> Context Term -> Term -> Either KernelError () }

-- | Forwards proofs.
--
-- Note that only @runForward@ is exported, and not the @Forward@
-- constructor. This enables us to know that any 'Forward' created outside
-- this module must constitute a valid proof, as it must have been formed
-- by piecing together trusted rules.
newtype Forward = Forward { runForward :: Context Type -> Context Term -> Either KernelError Term }

-- | Use the assumption @x@.
assumption :: Name -> Forward
assumption x = Forward \ctx hyps ->
  case Ctx.lookup x hyps of
    Just hyp -> pure hyp
    Nothing -> Left (AssumptionNotInScope ctx hyps x)

-- | Switch from backwards mode reasoning to forwards mode.
forward :: Forward -> Backward
forward fwdRule = Backward \ctx hyps goal -> do
  proves <- runForward fwdRule ctx hyps
  if goal == proves then
    pure ()
  else
    Left (GoalMismatch ctx hyps goal proves)

-- | Turn a backwards-mode rule into a forwards mode rule
-- by providing a goal hint.
hint :: Backward -> Check -> Forward
hint bwdRule propRule  = Forward \ctx hyps -> do
  prop <- runCheck propRule ctx Prop
  runBackward bwdRule ctx hyps prop
  pure prop

-- $true
-- ** True

topIntro :: Backward
topIntro = _topIntro

-- $false
-- ** False

botElim :: Backward -> Backward
botElim = _botElim

-- $and
-- ** Conjunctions

-- | Introduction rule for and.
andIntro :: Backward -> Backward -> Backward
andIntro = _andIntro

-- | Deduce @p@ from @p `and` q@.
andElimLeft :: Forward -> Forward
andElimLeft = _andElimLeft

-- | Deduce @q@ from @p `and` q@.
andElimRight :: Forward -> Forward
andElimRight = _andElimRight

-- $or
-- ** Disjunctions

orIntroLeft :: Backward -> Backward
orIntroLeft = _orIntroLeft

orIntroRight :: Backward -> Backward
orIntroRight = _orIntroRight

orElim :: Forward -> String -> (Name -> Forward) -> String -> (Name -> Backward) -> Forward
orElim = _orElim

-- $implies
-- ** Implication

impliesIntro :: String -> (Name -> Backward) -> Backward
impliesIntro = _impliesIntro

-- | Elimination rule for 'Implies'.
--
-- Use @p `implies` q@ and @p@ to deduce @q@.
impliesElim :: Forward -> Backward -> Forward
impliesElim = _impliesElim

-- $exists
-- ** Existentials

existsIntro :: Check -> Backward -> Backward
existsIntro = _existsIntro

existsElim :: Forward -> String -> String -> (Name -> Name -> Backward) -> Backward
existsElim = _existsElim

-- $forall
-- ** Universals

forAllIntro :: String -> (Name -> Backward) -> Backward
forAllIntro = _forAllIntro

forAllElim :: Forward -> Check -> Forward
forAllElim = _forAllElim

-- $equality
-- ** Equality

refl :: Backward
refl = Backward \ctx hyps goal ->
  case goal of
    Eq tm1 tm2 tp ->
      if tm1 == tm2 then
        pure ()
      else
        Left (TermMismatch ctx hyps tm1 tm2 tp)
    _ -> Left (IntroMismatch "refl" ctx hyps goal)

leibniz :: Forward -> String -> (Name -> Check) -> Backward -> Forward
leibniz eqRule str motRule motXRule = Forward \ctx hyps -> do
  proves <- runForward eqRule ctx hyps
  case proves of
    Eq tm1 tm2 tp -> do
      let (nm, motCtx) = Ctx.extend ctx str tp
      mot <- runCheck (motRule nm) motCtx Prop
      runBackward motXRule ctx hyps (subst mot nm tm2)
      pure (subst mot nm tm1)
    _ -> Left (ElimMismatch "leibniz" ctx hyps "equality" proves)

funext :: String -> (Name -> Backward) -> Backward
funext x bodyRule = Backward \ctx hyps goal ->
  case goal of
    Eq tm1 tm2 (Fn dom cod) -> do
      let (nm, ctx') = Ctx.extend ctx x dom
      runBackward (bodyRule nm) ctx' hyps (Eq (App tm1 (Var nm)) (App tm2 (Var nm)) cod)
    _ -> Left (IntroMismatch "funext" ctx hyps goal)

beta :: Backward -> Backward
beta eqRule = Backward \ctx hyps goal ->
  case goal of
    Eq (App (Lam (Binder x _ body)) arg) tm tp -> do
      runBackward eqRule ctx hyps (Eq (subst body x arg) tm tp)
    _ -> Left (IntroMismatch "beta" ctx hyps goal)

cong :: Forward -> Forward -> Forward
cong fnEqRule argEqRule = Forward \ctx hyps -> do
  provesFn <- runForward fnEqRule ctx hyps
  provesArg <- runForward argEqRule ctx hyps
  case (provesFn, provesArg) of
    (Eq f1 f2 (Fn dom cod), Eq a1 a2 dom') ->
      if dom == dom' then
        pure (Eq (App f1 a1) (App f2 a2) cod)
      else
        Left (TypeMismatch ctx dom a1 dom')
    (_, Eq {}) -> Left (ElimMismatch "cong" ctx hyps "equality" provesFn)
    _ -> Left (ElimMismatch "cong" ctx hyps "equality" provesArg)

propext :: Backward -> Backward -> Backward
propext toRule fromRule = Backward \ctx hyps goal ->
  case goal of
    Eq tm1 tm2 Prop -> do
      runBackward toRule ctx hyps (Implies tm1 tm2)
      runBackward fromRule ctx hyps (Implies tm2 tm1)
    _ -> Left (IntroMismatch "propext" ctx hyps goal)


-- * Tacticals

-- | Try to run @t1@, and backtrack and run @t2@ if @t1 failed.
instance Semigroup Backward where
  t1 <> t2 = Backward \ctx hyps goal ->
    case runBackward t1 ctx hyps goal of
      Right _ -> pure ()
      Left _ -> runBackward t2 ctx hyps goal

-- | Inspect the current proof state.
--
-- This makes it possible to write proof automation: try it out!
inspect :: (Context Type -> Context Term -> Term -> Backward) -> Backward
inspect k = Backward \ctx hyps goal ->
  runBackward (k ctx hyps goal) ctx hyps goal

-- | Stop the proof, and dump the proof state.
sorry :: String -> Backward
sorry msg = Backward \ctx hyps goal -> Left (Sorry msg ctx hyps goal)

-- * Pretty printng

prettyCtxLines :: (Pretty a) => Context a -> ShowS
prettyCtxLines = Pretty.vcat . fmap prettyCtxCell . Ctx.toList
  where
    prettyCtxCell (nm, a) = prettyPrecS Pretty.Isolated nm . Pretty.string " : " . prettyPrecS Pretty.Isolated a

instance Pretty KernelError where
  precOf _ = Pretty.Atom

  prettyPrecS _ (VariableNotInScope ctx nm) =
    Pretty.vcat
    [ Pretty.string "Error: variable scope."
    , Pretty.blank
    , Pretty.string "The variable " . prettyPrecS Pretty.Isolated nm . Pretty.string " is not in scope"
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    ]

  prettyPrecS _ (AssumptionNotInScope ctx hyps nm) =
    Pretty.vcat
    [ Pretty.string "Error: assumption scope."
    , Pretty.blank
    , Pretty.string "The assumption " . prettyPrecS Pretty.Isolated nm . Pretty.string " is not in scope"
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "------------------[Assumptions]------------------"
    , prettyCtxLines hyps
    ]

  prettyPrecS _ (CheckMismatch ruleName ctx goal) =
    Pretty.vcat
    [ Pretty.string "Error: check rule mismatch."
    , Pretty.blank
    , Pretty.string "The check rule " . Pretty.string ruleName . Pretty.string " does not apply to the current goal"
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "---------------------[Goal]----------------------"
    , prettyPrecS Pretty.Isolated goal
    ]
  prettyPrecS _ (InferMismatch ruleName ctx expected tm actual) =
    Pretty.vcat
    [ Pretty.string "Error: infer rule mismatch."
    , Pretty.blank
    , Pretty.string "When running the infer rule " . Pretty.string ruleName . Pretty.string " we expected a "
    , Pretty.string "  "  . Pretty.string expected
    , Pretty.string "but the term"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated tm
    , Pretty.string "has type"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated actual
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    ]
  prettyPrecS _ (TypeMismatch ctx expected tm actual) =
    Pretty.vcat
    [ Pretty.string "Error: type mismatch."
    , Pretty.blank
    , Pretty.string "Expected something of type"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated expected
    , Pretty.string "but the term"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated tm
    , Pretty.string "has type"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated actual
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    ]
  prettyPrecS _ (IntroMismatch ruleName ctx hyps goal) =
    Pretty.vcat
    [ Pretty.string "Error: forward rule mismatch."
    , Pretty.blank
    , Pretty.string "The forward rule " . Pretty.string ruleName . Pretty.string " does not apply to the current goal"
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "------------------[Assumptions]------------------"
    , prettyCtxLines hyps
    , Pretty.string "---------------------[Goal]----------------------"
    , prettyPrecS Pretty.Isolated goal
    ]
  prettyPrecS _ (ElimMismatch ruleName ctx hyps expected actual) =
    Pretty.vcat
    [ Pretty.string "Error: elim rule mismatch."
    , Pretty.blank
    , Pretty.string "When running the elim rule " . Pretty.string ruleName . Pretty.string " we expected a "
    , Pretty.string "  "  . Pretty.string expected
    , Pretty.string "but got a goal"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated actual
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "--------------------[Assumptions]--------------------"
    , prettyCtxLines hyps
    ]
  prettyPrecS _ (GoalMismatch ctx hyps expected actual) =
    Pretty.vcat
    [ Pretty.string "Error: goal mismatch."
    , Pretty.blank
    , Pretty.string "We were trying to prove the goal"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated expected
    , Pretty.string "but ended up proving"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated actual
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "--------------------[Assumptions]--------------------"
    , prettyCtxLines hyps
    ]
  prettyPrecS _ (TermMismatch ctx hyps tm1 tm2 tp) =
    Pretty.vcat
    [ Pretty.string "Error: term mismatch."
    , Pretty.blank
    , Pretty.string "Expected the two terms"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated tm1
    , Pretty.string "and"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated tm2
    , Pretty.string "of type"
    , Pretty.string "  " . prettyPrecS Pretty.Isolated tp
    , Pretty.string "to be equal, but they are not."
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "--------------------[Assumptions]--------------------"
    , prettyCtxLines hyps
    ]

  prettyPrecS _ (Sorry msg ctx hyps goal) =
    Pretty.vcat
    [ Pretty.string "Error: sorry."
    , Pretty.string msg
    , Pretty.blank
    , Pretty.string "--------------------[Context]--------------------"
    , prettyCtxLines ctx
    , Pretty.string "------------------[Assumptions]------------------"
    , prettyCtxLines hyps
    , Pretty.string "---------------------[Goal]----------------------"
    , prettyPrecS Pretty.Isolated goal
    ]
