{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A pure evaluator for our language.
--
-- This is a reference interpreter: you will not need to
-- modify any code, but it would be a good idea to carefully
-- read it!
module A5.Eval.Pure
  ( Val(..)
  , ComputeM(..)
  ) where

import Prelude hiding (not, and, or)
import Prelude qualified as P

import A5.Language
import A5.Data.Array.Pure (Array)
import A5.Data.Array.Pure qualified as Array

--------------------------------------------------------------------------------
-- Values

-- | Values.
data Val
  = VInt Int
  -- ^ An integer.
  | VBool Bool
  -- ^ A boolean.
  | VArray (Array Val)
  -- ^ An array
  deriving (Show, Eq)

-- | Types.
data Type
  = TpUnit
  | TpInt
  | TpBool
  | TpArray
  deriving (Show, Ord, Eq)

valType :: Val -> Type
valType (VInt _) = TpInt
valType (VBool _) = TpBool
valType (VArray _) = TpArray

--------------------------------------------------------------------------------
-- Computations

-- | A computation that is parameterised by a call stack, and that might fail.
newtype ComputeM a = ComputeM { runComputeM :: [String] -> Either String a }

instance Functor ComputeM where
  fmap f c = ComputeM (fmap f . runComputeM c)

instance Applicative ComputeM where
  pure a = ComputeM \_ -> pure a
  cf <*> ca = ComputeM \stack ->
    runComputeM cf stack <*> runComputeM ca stack

instance Monad ComputeM where
  return = pure
  c >>= f = ComputeM \stack -> do
    a <- runComputeM c stack
    runComputeM (f a) stack

-- | The 'MonadFail' instance of 'ComputeM' prints out an additional backtrace.
instance MonadFail ComputeM where
  fail msg = ComputeM \stack ->
    Left $ unlines (msg:"":"Backtrace:":stack)

-- | Push a frame to the call stack.
pushFrame :: String -> ComputeM a -> ComputeM a
pushFrame frame m = ComputeM \stack -> runComputeM m (frame:stack)

--------------------------------------------------------------------------------
-- Unwrappings

-- | Fail with a type error.
typeError :: Type -> Val -> ComputeM a
typeError expected v =
  fail $ "Type error: expected " <> prettyType expected <> " but got " <> prettyType (valType v)
  where
    prettyType :: Type -> String
    prettyType TpUnit = "unit"
    prettyType TpInt = "an int"
    prettyType TpBool = "a bool"
    prettyType TpArray = "an array"

-- | Get an integer value, and print, an error message if relevant.
unwrapInt :: Val -> ComputeM Int
unwrapInt (VInt i) = pure i
unwrapInt v = typeError TpInt v

-- | Get an boolean value, and print, an error message if relevant.
unwrapBool :: Val -> ComputeM Bool
unwrapBool (VBool b) = pure b
unwrapBool v = typeError TpBool v

-- | Get an array value, and print, an error message if relevant.
unwrapArray :: Val -> ComputeM (Array Val)
unwrapArray (VArray r) = pure r
unwrapArray v = typeError TpArray v

-- | Integer binops.
intBinOp :: String -> (Int -> Int -> Int) -> Val -> Val -> ComputeM Val
intBinOp name f x y = pushFrame name do
  vx <- unwrapInt x
  vy <- unwrapInt y
  pure (VInt (f vx vy))

-- | Integer comparisons.
intCmpOp :: String -> (Int -> Int -> Bool) -> Val -> Val -> ComputeM Val
intCmpOp name f x y = pushFrame name do
  vx <- unwrapInt x
  vy <- unwrapInt y
  pure (VBool (f vx vy))

-- | Unary boolean operations.
boolUnOp :: String -> (Bool -> Bool) -> Val -> ComputeM Val
boolUnOp name f x = pushFrame name do
  vx <- unwrapBool x
  pure (VBool (f vx))

-- | Boolean binops.
boolBinOp :: String -> (Bool -> Bool -> Bool) -> Val -> Val -> ComputeM Val
boolBinOp name f x y = pushFrame name do
  vx <- unwrapBool x
  vy <- unwrapBool y
  pure (VBool (f vx vy))

--------------------------------------------------------------------------------
-- Language

instance Lang Val (ComputeM Val) where
  int = VInt
  add = intBinOp "add" (P.+)
  sub = intBinOp "sub" (P.-)
  mul = intBinOp "add" (P.*)

  bool = VBool
  and = boolBinOp "and" (P.&&)
  or = boolBinOp "and" (P.||)
  xor = boolBinOp "and" (P./=)
  not = boolUnOp "not" P.not
  leq = intCmpOp "leq" (P.<=)
  lt = intCmpOp "lt" (P.<)

  array vs = VArray (Array.fromList vs)

  getArray arr i = pushFrame "getArray" do
    varr <- unwrapArray arr
    vi <- unwrapInt i
    maybe (fail "Index out of bounds.") pure $ Array.get vi varr


  setArray arr i v = pushFrame "setArray" do
    varr <- unwrapArray arr
    vi <- unwrapInt i
    maybe (fail "Index out of bounds in setArray") (pure . VArray) $ Array.set vi v varr

  lenArray arr = pushFrame "lenArray" do
    varr <- unwrapArray arr
    pure (VInt (length varr))

  let_ = (>>=)
  ret = pure

  forUp lo hi state body = pushFrame "forUp" do
    vlo <- unwrapInt lo
    vhi <- unwrapInt hi
    loop vlo vhi state
    where
      loop :: Int -> Int -> Val -> ComputeM Val
      loop vi vhi vstate
        | vi >= vhi = pure vstate
        | otherwise = body (VInt vi) vstate >>= loop (vi + 1) vhi

  forDown hi lo state body = pushFrame "forDown" do
    vhi <- unwrapInt hi
    vlo <- unwrapInt lo
    loop vhi vlo state
    where
      loop :: Int -> Int -> Val -> ComputeM Val
      loop vi vlo vstate
        | vi < vlo = pure vstate
        | otherwise = body (VInt vi) vstate >>= loop (vi - 1) vlo

  if_ b c1 c2 = pushFrame "if" do
    vb <- unwrapBool b
    if vb then c1 else c2

  crash = fail
