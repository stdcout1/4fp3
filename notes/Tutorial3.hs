-- functors 
-- we have map
-- but some times we want to use them with something else   
-- mapMaybe :: (a -> a) Maybe a -> Maybe b ...
-- mapTree :: (a -> a ) Tree a -> Tree a 

class MyFunctor f where 
    myFmap :: (a -> b) -> f a -> f b 

-- functors are types that can be mapped over 
--
-- has some important laws (contracts)
-- identity: fmap id = id 
-- composition: fmap f . map g = fmap (f . g) x

-- applicatives
class Functor f => MyApplicative f where -- applicatives are subclass of functors 
    myPure :: a -> f a 
    myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c

instance MyApplicative Maybe where 
    myPure x = Just x 
    myLiftA2 f (Just a) (Just b) = Just (f a b)
    myLiftA2 f _ _ = Nothing


--MONAD 

class Applicative m => MyMonad m where 
    myReturn :: a -> m a 
    myBind :: (a -> m b) -> m a -> m b
    -- vs fmap (a -> b) -> f a -> f b 
    -- here our fuction can return a b wrapped in m 

instance MyMonad Maybe where 
    myReturn = pure 
    myBind f (Just a ) = f a 
    myBind f Nothing = Nothing 





main :: IO ()
main = getLine >>= \x -> putStrLn ("Hello" ++ x) >>= \_ -> pure ()
