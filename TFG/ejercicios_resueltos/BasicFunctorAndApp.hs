{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative



------------------------------------------------------------------------
--      FIRST EXERCISE OF BASIC FUNCTOR AND APPLICATIVE SECTION       --
------------------------------------------------------------------------

data Tree a = Node a [Tree a]
 deriving (Eq, Show)


instance Functor Tree where
  fmap f (Node n ts)  = Node (f n) ( map (fmap f) ts )





mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node n [])     = Node (f n) []
mapTree f (Node n (t:ts)) = Node (f n) list
  where
    list  = (mapTree f t) : mapp (mapTree f) ts
    mapp _ [] = []
    mapp g (x:xs) = (g x):(mapp g xs)

-- NO HAY FORMA DE ESCRIBIR LOS DOS BUCLES RECURSIVOS EN UNA SOLA LINEA?

--  fmap f (Node n [])     = Node (f n) []
--  fmap f (Node n (t:ts)) = Node (f n) ????


n1 = Node 1 []
n2 = Node 2 []
n3 = Node 3 []
n4 = Node 4 []
n5 = Node 5 []
n6 = Node 6 []
n7 = Node 7 []

t1 = Node 10 [n1,n2,n3]
t2 = Node 11 [n4,n5]
t3 = Node 12 [n6]
t4 = Node 13 [n7]

t10 = Node 20 [t1,t2,t3,t4]


ej1 :: IO (Tree Int)
ej1 = do
  let t10' = fmap (100+) t10
  putStrLn (show t10)
  putStrLn (show t10')
  return t10'

-- Checking the correctness of the solution:
-- https://hackage.haskell.org/package/containers-0.5.7.1/docs/src/Data.Tree.html#line-74


------------------------------------------------------------------------


data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
  fmap _ (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)


-- Checking the laws:

-- fmap id (Left x) = Left x == Left x                       OK
-- fmap id (Right y) = Right (id y) = Right y == Right y     OK

-- fmap (f.g) (Left x) = Left x  == Left x = fmap f (Left x) = fmap f (fmap g (Left x))  OK
-- fmap (f.g) (Right y) = Right ((f.g) y)  
--   ==          --------------------------------> because   (f.g) y  ==  f (g y)        OK
--  Right (f (g y)) = fmap f (Right (g y)) = fmap f (fmap g (Right y))  


------------------------------------------------------------------------

type FuncsWithFixedDomain r = (->) r

-- *Main> :k FuncsWithFixedDomain
-- FuncsWithFixedDomain :: * -> * -> *
-- *Main> :k FuncsWithFixedDomain Int
-- FuncsWithFixedDomain Int :: * -> *


-- instance Functor (FuncsWithFixedDomain r) where  ==>  ERROR

-- [1 of 1] Compiling Main             ( BasicFunctorAndApp.hs, interpreted )

-- BasicFunctorAndApp.hs:88:10:
--     Duplicate instance declarations:
--       instance Functor (FuncsWithFixedDomain r)
--         -- Defined at BasicFunctorAndApp.hs:88:10
--       instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- Failed, modules loaded: none.

-- http://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.Base.html#line-612


mapF :: (a -> b) -> (r -> a) -> (r -> b)
mapF alpha f =  alpha . f


-- *Main> (mapF (2+) length ) [1,2,3]
-- 5
-- (0.00 secs, 0 bytes)


-- Checking the laws:

-- mapF id f = id . f  ==  f                                                           OK

-- mapF (alpha.beta) f = (alpha.beta).f  
--       ==          -------------------> because   (alpha.beta).f  == alpha.(beta.f)  OK
--  alpha.(beta.f) = mapF alpha (beta.f) = mapF alpha (mapF beta f)





------------------------------------------------------------------------
--     SECOND EXERCISE OF BASIC FUNCTOR AND APPLICATIVE SECTION       --
------------------------------------------------------------------------

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)


instance Applicative Maybe' where
  pure = Just'
  (Just' f) <*> (Just' x) = Just' (f x)
  _        <*> _          = Nothing'



-- Checking the laws:

-- Identity:
--   (pure id) <*> Nothing   ==  Nothing
--   (pure id) <*> (Just x) = (Just id) <*> (Just x) = Just (id x)  ==  Just x
-- Homomorphism
--   (pure f) <*> (pure x) = (Just f) <*> (Just x) == Just (f x) == pure (f x)
-- Interchange
--   Nothing <*> (pure x) = Nothing <*> (Just x) == Nothing ==  (Just ($ y)) <*> Nothing
--   (Just f) <*> (pure x) = Just (f x)  ==  Just (f $ x) = (Just ($ x)) <*> (Just f)
-- Composition
--   pure (.) <*> Just u <*> Just v <*> Just a = Just (u.v) <*> Just a  ==  Just ((u.v) a)
--     ==
--    Just u <*> Just (v a) = Just u <*> (Just v <*> Just a)




------------------------------------------------------------------------
--      THIRD EXERCISE OF BASIC FUNCTOR AND APPLICATIVE SECTION       --
------------------------------------------------------------------------


instance Applicative (Either' e) where
  pure x = Right' x
  (Right' f) <*> (Right' x) = Right' (f x)
  _          <*> (Left' x)  = Left' x


-- Checking the laws:
--   analogous to the Maybe instance


------------------------------------------------------------------------



-- instance Applicative (FuncsWithFixedDomain r) where
--   pure x = const x
--   (alpha <*> f) rVal = alpha r (f rVal)


-- http://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.Base.html#line-616




 