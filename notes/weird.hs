{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax        as THS

th1 :: ExpQ
th1 = [| 1 + 5 |]  -- quotation. ast equivlenet to 1 + 5

pp :: ExpQ -> IO ()
pp e = do
    expValue <- runQ e
    putStrLn $ pprint expValue

th2 :: ExpQ -> ExpQ 
th2 a = [| $a + 57 |] -- splce. take the ast of a and insert 

-- try pp $ th2 th1

-- we need to keep track of operational semantics and the algerbra to allow us 
-- to make simplfications in the complication step


