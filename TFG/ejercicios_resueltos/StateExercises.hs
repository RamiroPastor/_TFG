{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Control.Monad.State
import Control.Applicative
import System.Random



------------------------------------------------------------------------
--                FIRST EXERCISE OF THE STATE SECTION                 --
------------------------------------------------------------------------

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = (take n) <$> (getStdGen >>= f)
  where
    f = \ gen -> return (randomRs (1,6) gen)


{-

*Main> rollNDiceIO 10
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package bytestring-0.10.4.0 ... linking ... done.
Loading package Win32-2.3.0.2 ... linking ... done.
Loading package old-locale-1.0.0.6 ... linking ... done.
Loading package time-1.4.2 ... linking ... done.
Loading package random-1.0.1.1 ... linking ... done.
[5,5,1,6,6,5,1,6,5,3]
(0.11 secs, 5620896 bytes)
*Main> rollNDiceIO 6
[5,5,1,6,6,5]
(0.00 secs, 0 bytes)
*Main> rollNDiceIO 10
[5,5,1,6,6,5,1,6,5,3]
(0.02 secs, 0 bytes)
*Main> rollNDiceIO 10
[5,5,1,6,6,5,1,6,5,3]
(0.00 secs, 0 bytes)
*Main> getStdGen
700125431 1
(0.00 secs, 0 bytes)
*Main> getStdGen
700125431 1
(0.00 secs, 0 bytes)
*Main> newStdGen
895916699 2147483398
(0.00 secs, 0 bytes)
*Main> getStdGen
700125432 40692
(0.00 secs, 0 bytes)
*Main> getStdGen
700125432 40692
(0.00 secs, 0 bytes)
*Main> newStdGen
895956713 40691
(0.00 secs, 0 bytes)
*Main> getStdGen
700125433 1655838864
(0.00 secs, 0 bytes)
*Main> rollNDiceIO 10
[4,5,4,3,5,6,6,5,2,5]
(0.00 secs, 0 bytes)

-}


rollNDiceIO' :: Int -> IO [Int]
rollNDiceIO' n = sequence (fmap randomRIO (replicate n (1,6)))

-- does exactly the same




------------------------------------------------------------------------
--                SECOND EXERCISE OF THE STATE SECTION                --
------------------------------------------------------------------------

{-
roll2Dice :: StdGen -> ((Int,Int) , StdGen)
roll2Dice gen = ((n1,n2) , b) where
  (gen1, gen2) = split gen
  n1 = fst $ randomR (1,6) gen1
  n2 = fst $ randomR (1,6) gen2
  b  = snd $ randomR (1,6) gen2

This version seems correct but gives an "ambiguous type" error
-}


roll2Dice' :: StdGen -> ((Int,Int) , StdGen)
roll2Dice' gen = ((n1,n2) , b) where
  (gen1, gen2) = split gen
  n1 = fst $ aux gen1
  n2 = fst $ aux gen2
  b  = snd $ aux gen2
  aux = (\g -> randomR (1,6) g)  :: StdGen -> (Int,StdGen)


{-

*Main> roll2Dice' (mkStdGen 0)
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package bytestring-0.10.4.0 ... linking ... done.
Loading package Win32-2.3.0.2 ... linking ... done.
Loading package old-locale-1.0.0.6 ... linking ... done.
Loading package time-1.4.2 ... linking ... done.
Loading package random-1.0.1.1 ... linking ... done.
((6,1),1601120196 2147442707)
(0.08 secs, 6278672 bytes)
*Main> roll2Dice' (mkStdGen 0)
((6,1),1601120196 2147442707)
(0.02 secs, 0 bytes)
*Main> roll2Dice' (mkStdGen 38)
((6,2),166664317 2147442707)
(0.02 secs, 8181848 bytes)
*Main> roll2Dice' (mkStdGen 123)
((6,5),970416508 2147442707)
(0.00 secs, 0 bytes)
*Main> roll2Dice' (mkStdGen 3456789)
((2,1),1125608184 2147442707)
(0.00 secs, 0 bytes)
*Main> roll2Dice' (mkStdGen 4689273)
((3,4),1290960903 2147442707)
(0.00 secs, 0 bytes

-}


roll2Dice'' :: StdGen -> ((Int,Int) , StdGen)
roll2Dice'' gen = ((n1,n2) , res) where
  (n1,aux) = randomR (1,6) gen
  (n2,res) = randomR (1,6) aux 

-- *Main> roll2Dice' (mkStdGen 0)
-- ((6,1),1601120196 2147442707)
-- (0.00 secs, 0 bytes)
-- *Main> roll2Dice'' (mkStdGen 0)
-- ((6,6),1601120196 1655838864)
-- (0.02 secs, 8290224 bytes)




------------------------------------------------------------------------
--                THIRD EXERCISE OF THE STATE SECTION                 --
------------------------------------------------------------------------

rollNDice :: Int -> State StdGen [Int]
rollNDice n = state ( \gen -> ((take n) $ (randomRs (1,6) gen) , (snd.next) gen) )


{-

*Main> rollNDice 10

<interactive>:21:1:
    No instance for (Show (State StdGen [Int]))
      arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
(0.02 secs, 0 bytes)
*Main> runState (rollNDice 10) (mkStdGen 0)
Loading package transformers-0.4.3.0 ... linking ... done.
Loading package mtl-2.2.1 ... linking ... done.
([6,6,4,1,5,2,4,2,2,1],40014 40692)
(0.02 secs, 0 bytes)
*Main> runState (rollNDice 10) (mkStdGen 0)
([6,6,4,1,5,2,4,2,2,1],40014 40692)
(0.00 secs, 0 bytes)
*Main> runState (rollNDice 11) (mkStdGen 0)
([6,6,4,1,5,2,4,2,2,1,6],40014 40692)
(0.00 secs, 0 bytes)
*Main> runState (rollNDice 11) (mkStdGen 534621)
([3,1,5,6,2,4,3,3,6,1,2],2065012641 40692)
(0.00 secs, 0 bytes)

-}




------------------------------------------------------------------------
--               FOURTH EXERCISE OF THE STATE SECTION                 --
------------------------------------------------------------------------


-- instance Functor (State s) where

{-
StateExercises.hs:188:10:
    Illegal instance declaration for ‘Functor (State s)’
      (All instance types must be of the form (T t1 ... tn)
       where T is not a synonym.
       Use TypeSynonymInstances if you want to disable this.)
    In the instance declaration for ‘Functor (State s)’
-}

-- After adding that at the top of this file:

{-
StateExercises.hs:190:10:
    Illegal instance declaration for ‘Functor (State s)’
      (All instance types must be of the form (T a1 ... an)
       where a1 ... an are *distinct type variables*,
       and each type variable appears at most once in the instance head.
       Use FlexibleInstances if you want to disable this.)
    In the instance declaration for ‘Functor (State s)’
-}

-- Redefining the State data (to avoid these problems):

data State' s a = St (s -> (a,s))

instance Functor (State' s) where
  fmap f (St g) = St ( \s -> ( f $ fst $ g s , snd $ g s) )


-- fmap id x == x ??
-- fmap id (St g)
--   = St ( \s -> ( fst $ g s , snd $ g s) )
--   = St ( \s -> g s)
--   == (St g)

-- fmap (g.f) x == (fmap g) . (fmap f) x ??
-- fmap (g.f) (St pr)
--   = ST ( \s -> ((f.g) $ fst $ pr s , snd $ pr s) )
--   == fmap g (St (\s -> (f $ fst $ pr s , snd $ pr s))) 
--   = ((fmap g) . (fmap f)) x

-- This Functor instance works as follow:
--   fmap f (St pr) 
--     with  f :: a -> b , pr :: s -> (a,s)
--   applies f to the first value of the pair returned by pr




------------------------------------------------------------------------
--               FIFTH EXERCISE OF THE STATE SECTION                  --
------------------------------------------------------------------------

myModify :: (s -> s) -> State s ()
myModify f = state ( \s -> (() , f s) )

-- *Main> runState (myModify (2*)) 5
-- ((),10)
-- (0.00 secs, 0 bytes)



myGets :: (s -> a) -> State s a
myGets f = state ( \s -> (f s , s) )

-- *Main> runState (myGets (toEnum :: Int -> Char)) 90 
-- ('Z',90)
-- (0.00 secs, 0 bytes)




------------------------------------------------------------------------
--               SIXTH EXERCISE OF THE STATE SECTION                  --
------------------------------------------------------------------------


-- evalState :: State s a -> s -> a

allTypes :: State StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allTypes = liftM (,,,,,,) getRandom
                     `ap` getRandom
                     `ap` getRandom
                     `ap` getRandom
                     `ap` getRandom
                     `ap` getRandom
                     `ap` getRandom

getRandom :: Random a => State StdGen a
getRandom = state random



monsterRandom :: StdGen -> (Int, Float, Char, Integer, Double, Bool, Int)
monsterRandom gen = (n1,f1,c1,n2,d1,b1,n3)
  where
    (n1,g1) = random gen
    (f1,g2) = random g1
    (c1,g3) = random g2
    (n2,g4) = random g3
    (d1,g5) = random g4
    (b1,g6) = random g5
    (n3,_) = random g6



-- *Main> evalState allTypes (mkStdGen 0)
-- (-117157315039303149,0.4883204,'\260381',-2598893763451025729,0.30447780927171453,False,-5255441486919732037)
-- (0.02 secs, 0 bytes)
-- *Main> monsterRandom (mkStdGen 0)
-- (-117157315039303149,0.4883204,'\260381',-2598893763451025729,0.30447780927171453,False,-5255441486919732037)
-- (0.00 secs, 0 bytes)