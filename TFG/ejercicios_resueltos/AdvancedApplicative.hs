import Control.Applicative
import Control.Monad


------------------------------------------------------------------------
--   FIRST EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION     --
------------------------------------------------------------------------


-- sequence :: Monad m => [m a] -> m [a]

-- Looking at the type signature, the expected behavior could be:
-- sequence [Just 5, Just 6, Just 7, Nothing] == Just [5,6,7]
-- sequence [Nothing, Nothing, Nothing] == Nothing

-- However, taking a closer look, 
-- sequence :: Monad m => [m a] -> m [a]
-- sequence = mapM id
--
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM f as = foldr k (return []) as
--   where
--     k a r = do { x <- f a; xs <- r; return (x:xs) }

-- The base case of the foldr call inside mapM is (return []), so it should be:
-- sequence [Nothing, Nothing, Nothing] == Just []


-- Finally, the real behavior is:
-- *Main> sequence [Nothing, Nothing, Nothing]
-- Nothing
-- (0.00 secs, 0 bytes)
-- *Main> sequence [Nothing, Nothing, Nothing, Just 5, Just 7]
-- Nothing
-- (0.00 secs, 0 bytes)
-- *Main> sequence [Just 5, Just 7]
-- Just [5,7]
-- (0.00 secs, 0 bytes)

-- To understand this:
--   in (MapM id) ,  we have  id :: m b -> m b
--   essentially, as soon as we get a Nothing in the do block, we have  Nothing >>= ... 
--   which always ends up being Nothing




------------------------------------------------------------------------
--   SECOND EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION    --
------------------------------------------------------------------------



-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- Recalling:
-- (>>=) :: Monad f => f a -> (a -> f b) -> f b 
-- fmap  :: Functor f => (a -> b) -> f a -> f b


myApply :: (Functor m, Monad m) => m (a -> b) -> m a -> m b
myApply phi m =   phi >>= (\f -> fmap f m) 


-- *Main> myApply (Just (2+)) (Just 3)
-- Just 5

-- *Main> myApply [(1+), (2*), id] [10,20,30]
-- [11,21,31,20,40,60,10,20,30]




------------------------------------------------------------------------
--   THIRD EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION     --
------------------------------------------------------------------------


liftA5 :: Applicative f => (a -> b -> c -> d -> e -> k)
  -> f a -> f b -> f c -> f d -> f e -> f k

liftA5 func a b c d e = fmap func a <*> b <*> c <*> d <*> e
--                      (          )     )     )     )     )




------------------------------------------------------------------------
--   FOURTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION    --
------------------------------------------------------------------------


myListApply :: [ a -> b ] -> [a] -> [b]
myListApply fs as  =  concatMap (\f -> map f as) fs


-- *Main> myListApply [(1+), (2*), id] [10,20,30]
-- [11,21,31,20,40,60,10,20,30]


fs <|*|> xs = concatMap (\x -> fmap ($ x) fs) xs




------------------------------------------------------------------------
--   FIFTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION     --
------------------------------------------------------------------------


-- Commutativity:
--   liftA2 f u v  ==  liftA2 (flip f) v u
--  or equivalently
--   f <$> u <*> v  ==  flip f <$> v <*> u

-- Or equivalently:
--   do {x <- u; y <- v; return (f x y)} 
--     ==
--   do {y <- v; x <- u; return ((flip f) y x)}
--     ==
--   do {y <- v; x <- u; return (f x y)}


-- *Main> let aux f u v = do {x <- u; y <- v; return (f x y)}
-- (0.02 secs, 0 bytes)
-- *Main> :t aux
-- aux :: Monad m => (t -> t1 -> b) -> m t -> m t1 -> m b
-- *Main> :t liftA2
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- *Main> :t liftM2
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r




------------------------------------------------------------------------
--   SIXTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION     --
------------------------------------------------------------------------


-- Is  ZipList  (found in Control.Applicative) a commutative applicative functor?
--   newtype ZipList a = ZipList { getZipList :: [a] }
--   instance Applicative ZipList where
--     (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
--     pure x                        = ZipList (repeat x)

-- (f <$> (ZipList l1)) <*> (ZipList l2)
--   ==  (ZipList (map f l1)) <*> ZipList l2
--   ==  ZipList (zipWith ($) (map f l1) l2)
--   ==  ZipList (zipWith ($) (map (flip f) l2) l1)
--   ==  (ZipList (map (flip f) l2)) <*> ZipList l1
--   == ( (flip f) <$> (ZipList l2) ) <*> (ZipList l1)


-- Is  ((->) r)  a commutative applicative functor?
-- instance Applicative ((->) a) where
--     pure = const
--     (<*>) f g x = f x (g x)

-- We have 
--   f :: a -> b -> c
--   g :: r -> a
--   h :: r -> b
-- So
-- (f <$> g <*> h) x
--   ==  ((f.g) <*> h) x
--   ==  (f.g) x (h x)
--   ==  ((flip f).h) x (g x)
--   ==  ( ((flip f).h) <*>  g ) x
--   ==  ( ((flip f) <$> h) <*> g ) x


-- Is  State s  a commutative applicative functor?
--   No, because the order of computations affects the result

-- https://en.wikibooks.org/wiki/Haskell/Solutions/Applicative_functors



------------------------------------------------------------------------
--  SEVENTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION    --
------------------------------------------------------------------------


-- [2,7,8] *> [3,9] ?

-- *Main> :t (*>)
-- (*>) :: Applicative f => f a -> f b -> f b
-- *Main> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- *Main> :t ($>)

-- <interactive>:1:1:
--     Not in scope: ‘$>’
--     Perhaps you meant one of these:
--       ‘>>’ (imported from Control.Monad), ‘$!’ (imported from Prelude),
--       ‘>’ (imported from Prelude)
-- *Main> :t (>>)
-- (>>) :: Monad m => m a -> m b -> m b

-- So it could be:
--   [2,7,8] *> [3,9]  ==  [3,9]
-- However:
--   *Main> [2,7,8] *> [3,9]
--   [3,9,3,9,3,9]
--   (0.02 secs, 0 bytes)
--   *Main>  [2,7,8] >> [3,9]
--   [3,9,3,9,3,9]
--   (0.00 secs, 0 bytes)
-- Because:
--   (*>) u v = pure (const id) <*> u <*> v
-- *Main> :t const
-- const :: a -> b -> a
-- *Main> :t id
-- id :: a -> a
-- *Main> :t (const id)
-- (const id) :: b -> a -> a
-- So:
--   [2,7,8] *> [3,9]
--   ==  [ (const id) ] <*> [2,7,8] <*> [3,9]
--   ==  [const id 2 , const id 7 , const id 8 ] <*> [3,9]
--   ==  [const id 2 3 , const id 2 9 , ... , const id 8 9]
--   ==  [3,9,3,9,3,9]

-- *Main> [2,7,8] <* [3,9]
-- [2,2,7,7,8,8]
-- (0.00 secs, 0 bytes)
-- *Main> [3,9] *> [2,7,8]
-- [2,7,8,2,7,8]
-- (0.00 secs, 0 bytes)

-- In conclusion, for lists:
--   l1 (*>) l2  repeats l2 as many times as length l1
--   l1 (<*) l2  repeats each element of l1 as many times as length l2




------------------------------------------------------------------------
--   EIGHTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION    --
------------------------------------------------------------------------


-- (<**>) :: Applicative f => f a -> f (a -> b) -> f b
--   Recalling:  (<**>)  is NOT  flip (<*>)


myInvertedApply :: Applicative f => f a -> f (a -> b) -> f b
myInvertedApply = liftA2 (flip ($))


-- (searched hoogle because i was lazy)

-- Recalling:
-- Commutativity:
--   liftA2 f u v  ==  liftA2 (flip f) v u




------------------------------------------------------------------------
--    NINTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION    --
------------------------------------------------------------------------


-- from the Haskell documentation:
--   (=<<) :: Monad m => (a -> m b) -> m a -> m b
--   f =<< x         = x >>= f

-- *Main> :t (*>)
-- (*>) :: Applicative f => f a -> f b -> f b
-- *Main> :t (<*)
-- (<*) :: Applicative f => f a -> f b -> f a
-- *Main> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- *Main> :t (<**>)
-- (<**>) :: Applicative f => f a -> f (a -> b) -> f b

-- It does not happen because the order of computations is fixed:
--   First, the monadic action x
--   Second, retrieve the hidden value in x and apply f
-- It doesn't make sense to think that we can evaluate any monadic action in f
-- first, because we need a value  a  from  m a  before




------------------------------------------------------------------------
--    TENTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION    --
------------------------------------------------------------------------


data AT a = L a | B (AT a) (AT a)
  deriving (Show)

at1 :: AT Int
at1 = B (L 5) (B (L 2) (L 100))

at2 :: AT (Int -> Char)
at2 = let f=toEnum.(100+) in B (B (L f) (L f)) (B (L f) (B (L f) (L f)))

at3 :: AT (Int -> Int)
at3 = B (L (2*)) ( B (L (1+)) (L (3*)) )


-- Working with finite trees


mapAT :: (a -> b) -> AT a -> AT b
mapAT f (L a) = L (f a)
mapAT f (B l r) = B (mapAT f l) (mapAT f r)


instance Functor AT where
  fmap = mapAT

-- fmap id (L x) = L (id x)  ==  L x
-- fmap id (B l r) = B (fmap id l) (fmap id r)  ==  B l r 
--   should be proved by induction (over the depth of the tree?)

-- fmap (f.g) (L x) = L ( f (g x) )
--   ==
-- fmap f (L (g x)) = (fmap f).(fmap g) (L x)



-- For each leaf in the tree of functions, substitute it with the tree
-- resulting from applying that function to the tree of values:
applyAT :: AT (a -> b) -> AT a -> AT b
applyAT at_fs at_xs = case at_fs of 
  (L f)   -> mapAT f at_xs
  (B l r) -> B (applyAT l at_xs) (applyAT r at_xs)


instance Applicative AT where
  pure x = L x
  fs <*> xs = applyAT fs xs

-- pure id <*> at 
--   = (L id) <*> at 
--   = mapAT id at
--   = at           (checked in the Functor part)
--   == at          (as we wanted)

-- (pure f) <*> (pure x) 
--   = (L f) <*> (L x)
--   = mapAT f (L x)
--   = L (f x)
--   == pure (f x)  (as we wanted)

-- pure (.) <*> gs <*> fs <*> xs
--   ==   ????
-- gs <*> (fs <*> xs) 
--
-- pure (.) <*> gs <*> fs <*> xs
--   = (L (.)) <*> gs <*> fs <*> xs
--   = (mapAT (.) gs) <*> fs <*> xs
--   = case gs of
--     (L g) =>
--       = L (g.) <*> fs <*> xs
--         = case fs of
--           (L f) =>
--             = L (g.f) <*> xs
--             = mapAT (g.f) xs
--             == mapAT g (mapAT f xs)
--               = gs <*> (fs <*> xs)
-- and the other cases are `trivial':
--   applyAT (B l r) xs = B (applyAT l xs) (applyAT r xs)
-- so everything is decided in the leaves

-- fs <*> (pure y)
--   = fs <*> (L y)
--   == substitute each leaf (L f) in fs by [L (f y)]
--   = mapAT ($ y) fs
--   = L ($ y) <*> fs 
--   = pure ($ y) <*> fs



bindAT :: AT a -> (a -> AT b) -> AT b
bindAT (L x) f   = f x
bindAT (B l r) f = B (bindAT l f) (bindAT r f)


instance Monad AT where
  return x = (L x)
  (>>=)    = bindAT


-- return a >>= f
--   = (L a) >>= f
--   = f a         (by the def of >>=)
--   == f a        (as we wanted)

-- at >>= return
--   = case at of
--     (L a) =>
--       = (L a) >>= return
--       = return a
--       == (L a)  (as we wanted)
--     (B l r) =>
--       = B (l>>=return) (r>>=return)
--         (reduced to the leaf case)

-- at >>= (\x -> f x >>= g)
--   ==   ????
-- (at >>= f) >>= g
--
-- case at of (L a):
--   = (L a) >>= (\x -> f x >>= g)
--   = (\x -> f x >>= g) a
--   = (f a) >>= g
--   == (f a) >>= g
--   = ((L a) >>= f) >>= g
--   = (at >>= f) >>= g


-- Is (<*>) equal to ap?
--   ap :: (Monad m) => m (a -> b) -> m a -> m b
--   ap m1 m2 = do {x1 <- m1; x2 <- m2; return (x1 x2)}
-- Translating the do block first
--   ap m1 m2  =  m1 >>= ( \x1 -> m2 >>= \x2 -> return (x1 x2) )
-- So:
--   ap at_fs at_xs
--     = at_fs >>= ( \x1 -> at_xs >>= \x2 -> return (x1 x2) )
--     case  at_fs of (L f)
--       = ( \x1 -> at_xs >>= \x2 -> return (x1 x2) ) f
--       = at_xs >>= (\x2 -> return (f x2))
--         case at_xs of (L x)
--           = (\x2 -> return (f x2)) x
--           = return (f x)
--           = L (f x)
--           == (L f) <*> (L x)




------------------------------------------------------------------------
--  ELEVENTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION   --
------------------------------------------------------------------------


-- Grow the tree by replacing each leaf with a branch
-- containing two copies of the leaf.

fructify1 :: AT a -> AT a
fructify1 at =  at >>= (\a -> B (L a) (L a))

fructify1' :: AT a -> AT a
fructify1' at =  join ( fmap (\a -> B (L a) (L a)) at )

fructify2 :: AT a -> AT a
fructify2 at =  (fmap g at) <*> (B (L 0) (L 0))
  where g a = \_ -> a

fructify2' :: AT a -> AT a
fructify2' at =  (fmap const at) <*> (B (L "hi") (L "bye"))

fructify2'' :: AT a -> AT a
fructify2'' at = at <* (B (L True) (L False))

wrongFructify at = at *> (B (L 0) (L 0))



-- Replace a branch of a tree with a leaf carrying the default 
-- value z whenever any of the leaves directly on it satisfies the test p

prune :: a -> (a -> Bool) -> AT a -> AT a
-- prune z p t = boolT ???
--   where boolT = fmap p t
-- None of the instances above allows to cut parts of the tree because
-- each of them only grows the tree(s). 
prune z p (L x)              = if p x then (L z) else (L x)
prune z p t@(B (L x) (L y) ) = if (p x) || (p y) then (L z) else t
prune z p (B (L x) t)        = if p x then (L z) else B (L x) (prune z p t)
prune z p (B t (L y))        = if p y then (L z) else B (prune z p t) (L y)
prune z p (B l r)            = B (prune z p l) (prune z p r)


-- *Main> at1
-- B (L 5) (B (L 2) (L 100))
-- (0.00 secs, 12846520 bytes)
-- *Main> prune (-1) (<3) at1
-- B (L 5) (L (-1))
-- (0.00 secs, 0 bytes)
-- *Main> prune (-1) (<10) at1
-- L (-1)
-- (0.00 secs, 0 bytes)



-- Duplicate a tree applying two different functions

reproduce :: (a -> b) -> (a -> b) -> AT a -> AT b
reproduce f g t = B (fmap f t) (fmap g t)

reproduce2 :: (a -> b) -> (a -> b) -> AT a -> AT b
reproduce2 f g t = (B (L f) (L g)) <*> t

reproduce2' :: (a -> b) -> (a -> b) -> AT a -> AT b
reproduce2' f g t =  (B (L f) (L g)) >>= (\f -> fmap f t)

reproduce2'' :: (a -> b) -> (a -> b) -> AT a -> AT b
reproduce2'' f g t = (B (L f) (L g)) `ap` t




------------------------------------------------------------------------
--   TWELFTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION   --
------------------------------------------------------------------------

-- First, the reversed sequencing version of (<*>)
--   (first, search for the leaves in the tree of values
--    second, pass each value to every function in the tree of funcs)
applyAT'' :: AT (a -> b) -> AT a -> AT b
applyAT'' at_fs (L x) = mapAT ($ x) at_fs
applyAT'' at_fs (B l r) = B (applyAT'' at_fs l) (applyAT'' at_fs r)

-- *Main> applyAT at3 at1
-- B (B (L 10) (B (L 4) (L 200))) (B (B (L 6) (B (L 3) (L 101))) (B (L 15) (B (L 6) (L 300))))
-- (0.00 secs, 0 bytes)
-- *Main> applyAT'' at3 at1
-- B (B (L 10) (B (L 6) (L 15))) (B (B (L 4) (B (L 3) (L 6))) (B (L 200) (B (L 101) (L 300))))
-- (0.00 secs, 0 bytes)


-- And the other version:

applyAT' :: AT (a -> b) -> AT a -> AT b
applyAT' (L f) at   = mapAT f at
applyAT' (B l r) at = B (applyAT' r at) (applyAT' l at)


-- *Main> applyAT at3 at1
-- B (B (L 10) (B (L 4) (L 200))) (B (B (L 6) (B (L 3) (L 101))) (B (L 15) (B (L 6) (L 300))))
-- (0.00 secs, 0 bytes)
-- *Main> applyAT' at3 at1
-- B (B (B (L 15) (B (L 6) (L 300))) (B (L 6) (B (L 3) (L 101)))) (B (L 10) (B (L 4) (L 200)))
-- (0.00 secs, 0 bytes)

-- pure id <*> t == t ??
-- pure id <*> t
--   = (L id) <*> t
--   = mapAT f t
--   == t

-- pure f <*> pure x == pure (f x) ??
-- pure f <*> pure x
--   = (L f) <*> (L x)
--   = mapAT f (L x) 
--   = L (f x)
--   == pure (f x)

-- fs <*> pure x == pure ($ x) <*> fs ??
-- fs <*> pure x == pure ($ x) <*> fs
--   iff fs <*> (L x) == L ($ x) <*> fs
--   iff fs <*> (L x) == mapAT ($ x) fs
-- Now, fs <*> (L x) =
--   case fs of
--     (L f):
--       = (L f) <*> (L x)
--       = L (f x) 
--       == L ($ x) <*> (L f)
--     (B l r):
--       = (B l r) <*> (L x)
--       = B (r <*> (L x)) (l <*> (L x))
--       =/= B (mapAT ($ x) l) (mapAT ($ x) r) ?!?!?!?!?
--       = mapAT ($ x) (B l r)
--       = L ($ x) <*> (B l r)

-- To solve that problem, i thought about: 
-- -- First, we need an illegal version of fmap:

mapAT' :: (a -> b) -> AT a -> AT b
mapAT' f (L x)   = L (f x)
mapAT' f (B l r) = B (mapAT' f r) (mapAT' f l)

-- Occurs that 
-- mapAT' id (B (L 1) (L 2)) 
--   = B (mapAT' id (L 2)) (mapAT' id (L 1))
--   = B (L 2) (L 1)
--   =/= B (L 1) (L 2)
-- However, the applicative instance seems ok   <-- NO:
--   pure id <*> t  =/=  t

-- So i went to
-- https://en.wikibooks.org/wiki/Haskell/Solutions/Applicative_functors#111
-- And found:

-- instance Applicative AT where
--     pure x                  = L x
--     L f       <*> tx        = fmap f tx
--     tf        <*> L x       = fmap ($ x) tf
--     B tfl tfr <*> B txl txr = B (tfl <*> txl) (tfr <*> txr)

-- " It only combines subtrees with matching positions in the tree structures. 
-- The resulting behaviour is similar to that of ZipLists, 
-- except that when the subtree shapes are different:
-- it inserts missing branches rather than removing extra ones 
-- (and it couldn't be otherwise, since there are no empty ATs). 
-- By the way, sagittalMap would have the exact same implementation of reproduce, 
-- only using this instance. "

-- And seems ok:

-- pure id <*> t == t  OK
-- pure f <*> pure x == pure (f x)  OK
-- fs <*> pure x == pure ($ x) <*> fs  OK
-- pure (.) <*> gs <*> fs <*> as == gs <*> (fs <*> as)  OK


data AT' a = L' a | B' (AT' a) (AT' a)
  deriving (Show)

at1' :: AT' Int
at1' = B' (L' 5) (B' (L' 2) (L' 100))

at2' :: AT' (Int -> Char)
at2' = let f=toEnum.(100+) in B' (B' (L' f) (L' f)) (B' (L' f) (B' (L' f) (L' f)))

at3' :: AT' (Int -> Int)
at3' = B' (L' (2*)) ( B' (L' (1+)) (L' (3*)) )   


instance Functor AT' where
  fmap f (L' x) = L' (f x)
  fmap f (B' l r) = B' (fmap f l) (fmap f r)

instance Applicative AT' where
  pure = L'
  L' f       <*> tx        = fmap f tx
  tf        <*> L' x       = fmap ($ x) tf
  B' tfl tfr <*> B' txl txr = B' (tfl <*> txl) (tfr <*> txr)



-- *Main> at3 <*> at1
-- B (B (L 10) (B (L 4) (L 200))) (B (B (L 6) (B (L 3) (L 101))) (B (L 15) (B (L 6) (L 300))))
-- (0.00 secs, 0 bytes)
-- *Main> at3' <*> at1'
-- B' (L' 10) (B' (L' 3) (L' 300))
-- (0.02 secs, 0 bytes)




------------------------------------------------------------------------
-- THIRTEENTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION  --
------------------------------------------------------------------------

-- Recalling:
--   unit  :: Monoidal f => f ()
--   (*&*) :: Monoidal f => f a -> f b -> f (a,b)
--   pure  :: Applicative f => a -> f a
--   (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- IMPLEMENTING THE MONOIDAL CLASS:
-- (because its not defined in the Prelude)

class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)


myUnit :: Applicative f => f ()
myUnit = pure ()

myOP :: Applicative f => f a -> f b -> f (a,b)
myOP a b = (fmap (,) a) <*> b


-- *Main> myOP [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- (0.02 secs, 0 bytes)


instance Monoidal [] where
  unit = [()]
  (*&*) l1 l2 = concatMap (((flip zip) l2) . repeat) l1 

-- *Main> [1,2,3] *&* [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
   


pureGivenMonoidal :: Monoidal f => a -> f a
pureGivenMonoidal x = fmap (\_ -> x) unit

applyGivenMonoidal :: Monoidal f => f (a -> b) -> f a -> f b
applyGivenMonoidal fs as = fmap (\(f,a) -> f a) (fs *&* as)




------------------------------------------------------------------------
-- FOURTEENTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION  --
------------------------------------------------------------------------

-- Commutativity:
--   liftA2 f u v  ==  liftA2 (flip f) v u
--  or equivalently
--   f <$> u <*> v  ==  flip f <$> v <*> u
-- Or equivalently:
--   do {x <- u; y <- v; return (f x y)} 
--     ==
--   do {y <- v; x <- u; return ((flip f) y x)}
--     ==
--   do {y <- v; x <- u; return (f x y)}

-- Or equivalently:
--   fmap (\(f,a) -> f a) ((f <$> u) *&* v) 
--     ==
--   fmap (\(f,a) -> f a) ((flip f) <$> v *&* u)




------------------------------------------------------------------------
--  FIFTEENTH EXERCISE OF THE ADVANCED MONAD AND APPLICATIVE SECTION  --
------------------------------------------------------------------------

instance Monoidal ZipList where
  unit = ZipList [()]
  (*&*) (ZipList l1) (ZipList l2) = ZipList (zip l1 l2)


-- *Main> (,) <$> (ZipList [1,2,3,4]) <*> (ZipList [10,11,12])
-- ZipList {getZipList = [(1,10),(2,11),(3,12)]}
-- (0.00 secs, 0 bytes)
-- *Main> (ZipList [1,2,3,4]) *&* (ZipList [10,11,12])
-- ZipList {getZipList = [(1,10),(2,11),(3,12)]}
-- (0.00 secs, 0 bytes)



instance Monoidal ((->) r) where
  unit = const ()
  (*&*) f g = \r -> (f r, g r)


-- *Main> ((,) <$> (\n -> 2*n) <*> (\n -> 3*n)) 5
-- (10,15)
-- (0.00 secs, 0 bytes)
-- *Main> ((\n -> 2*n) *&* (\n -> 3*n)) 5
-- (10,15)
-- (0.00 secs, 0 bytes)


