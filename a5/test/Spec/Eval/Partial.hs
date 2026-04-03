-- | Spec for @A5.Eval.Partial@.
module Spec.Eval.Partial
  ( evalPartialSpec,
  )
where

import A5.Compile
import A5.Data.Array.Pure qualified as Array
import A5.Eval.Partial
import A5.Language
import Spec.Programs
import Test.Tasty
import Test.Tasty.HUnit

assertResidualizesTo :: GluedProg Expr (GluedVal Expr) -> FreshM Prog -> Assertion
assertResidualizesTo c expected = do
  runFreshM (residualProg c) 0 @?= runFreshM expected 0

evalPartialSpec :: TestTree
evalPartialSpec =
  testGroup
    "Eval.Pure"
    [ testCase "plus 2 5 == 7" $
        assertResidualizesTo (plus (staticInt 2) (staticInt 5)) $
          ret (int 7),
      testCase "plus 2 x == x + 2" $
        let x = Var (User "x")
         in assertResidualizesTo (plus (staticInt 2) (dynamic x TpInt)) $
              x `add` int 2,
      testCase "time 2 5 == 10 " $
        assertResidualizesTo (times (staticInt 2) (staticInt 5)) $
          ret (int 10),
      testCase "time 2 x == x * 2" $
        let x = Var (User "x")
         in assertResidualizesTo (times (staticInt 2) (dynamic (Var (User "x")) TpInt)) $
              x `mul` int 2,
      testCase "power x 4 == let y = x * x in y * y" $
        let x = Var (User "x")
         in assertResidualizesTo (power (dynamic (Var (User "x")) TpInt) (staticInt 4)) $
              let_ (x `mul` x) \xSquare ->
                xSquare `mul` xSquare,
      testCase "insertionSort [x, y] == if (y < x) then [y, x] else [x, y]" $
        let x = Var (User "x")
            y = Var (User "y")
         in assertResidualizesTo (insertionSort $ staticArray $ Array.fromList [dynamic x TpInt, dynamic y TpInt]) $
              let_ (lt y x) \cmp ->
                if_
                  cmp
                  (ret $ array [y, x])
                  (ret $ array [x, y]),
      testCase "insertionSort [2, x, 1]" $
        let x = Var (User "x")
            xs = [staticInt 2, dynamic x TpInt, staticInt 1]
         in assertResidualizesTo (insertionSort $ staticArray $ Array.fromList xs) $
              let_ (x `lt` int 2) \cmpx2 ->
                if_
                  cmpx2
                  ( let_ (int 1 `lt` x) \cmpx1 ->
                      if_
                        cmpx1
                        (ret $ array [int 1, x, int 2])
                        (ret $ array [x, int 1, int 2])
                  )
                  -- A more clever implementation would be able to eliminate this
                  -- branch: why can we eliminate it, and what strategies could we
                  -- use to extend our partial evaluator to handle this?
                  ( let_ (int 1 `lt` x) \cmpx1 ->
                      if_
                        cmpx1
                        (ret $ array [int 1, int 2, x])
                        ( let_ (x `lt` int 2) \absurd ->
                            if_
                              absurd
                              (ret $ array [x, int 2, int 1])
                              (ret $ array [int 2, x, int 1])
                        )
                  ),
      testCase "alwaysCrash x == crash" $
        let x = dynamic (Var (User "x")) TpBool
         in assertResidualizesTo (alwaysCrash x) $
              crash $
                unlines
                  [ "type error.",
                    "Expected a bool in the second argument of add",
                    "but got something of type bool"
                  ],
      testCase "add (add x 3) 4 == x + 7 (cnst folding)" $
        let x = Var (User "x")
         in assertResidualizesTo (do v <- add (dynamic x TpInt) (staticInt 3); add v (staticInt 4)) $
              x `add` int 7,
      -- polynomial properties
      testCase "sub x x == 0 (cancellation)" $
        let x = Var (User "x")
            dx = dynamic x TpInt
         in assertResidualizesTo (sub dx dx) $
              ret (int 0),
      testCase "mul x 0 == 0 (annilhator)" $
        let x = Var (User "x")
         in assertResidualizesTo (mul (dynamic x TpInt) (staticInt 0)) $
              ret (int 0),
      -- annilation and caccnelation are implemented the same as int so we dont need to test them
      testCase "not (not x) == x (double negation)" $
        let x = Var (User "x")
         in assertResidualizesTo (do v <- A5.Language.not (dynamic x TpBool); A5.Language.not v) $
              ret x,
      -- simple branch elim on constant b
      testCase "if_ (bool False) a b == b " $
        let x = Var (User "x")
         in assertResidualizesTo (if_ (staticBool False) (ret (staticInt 99)) (ret (dynamic x TpInt))) $
              ret x,
      -- static array ops
      testCase "getArray [67, 69, 420] 1 == 69 "
        $ assertResidualizesTo
          (getArray (staticArray $ Array.fromList [staticInt 67, staticInt 69, staticInt 420]) (staticInt 1))
        $ ret (int 69),
      testCase "setArray [1, 2, 3] 0 67 == [67, 2, 3] "
        $ assertResidualizesTo
          ( do
              arr <-
                setArray
                  (staticArray $ Array.fromList [staticInt 1, staticInt 2, staticInt 3])
                  (staticInt 0)
                  (staticInt 67)
              ret arr
          )
        $ ret (array [int 67, int 2, int 3]),
      testCase "lenArray [1, 2, 3] == 3 "
        $ assertResidualizesTo
          (lenArray (staticArray $ Array.fromList [staticInt 1, staticInt 2, staticInt 3]))
        $ ret (int 3),
      -- loop unroll
      testCase "forUp with static bounds fully unrolls" $
        -- sum from 0 to 2: acc + 0 + 1 + 2 = x + 3
        let x = Var (User "x")
         in assertResidualizesTo
              (forUp (staticInt 0) (staticInt 3) (dynamic x TpInt) \i acc -> add acc i)
              $ x `add` int 3
    ]

-- some analysis:
-- what its good at:
-- all types of constant folding. because we implemented it using a algebraic data structure, we get it all for free
-- also in the future when we have some operations set and stuff we can do lattices and inherit more
-- intersting properties for free.
-- obvious we have branch elimination and loop unfolding which can prevent
-- long runtimes of trivial operations
-- bads:
-- it dosnt track any inforamtion. for example after a branch elim we know
-- that the B is false but it dosnt store that so its not as optimal consider:
-- insertionSort [2, x, 1]:
--        let_ (x `lt` int 2) \absurd ->
--        if_ absurd (ret [x, 2, 1]) (ret [2, x, 1])
--
--  Here "absurd" is always False (we're already in the x >= 2
--  branch), but the evaluator doesn't know that. to handle this 
--  we could have a ctx for cmp operations and check the context everytime we come accross 
--  one of them.
--
-- we also have type information loss through GUnknown. when a value passes
-- through a dynamic binding (the accumulatr in a loop
-- with dynamic bounds), it becomes GUnknown, losing all type
-- information. this prevents further static operations on it
-- even if its type was previously known. i pointed it out in my code in some places
-- also there is no common subexpression elimination! when you make a compiler that is one of the first optimization you learn 
-- in 3tb3. it can also be done on a single pass compiler like this
-- the first improvement i would add is common subexpression elimination.
