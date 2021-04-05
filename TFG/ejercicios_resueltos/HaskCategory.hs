

------------------------------------------------------------------------
--            FIRST EXERCISE OF THE HASK CATEGORY SECTION             --
------------------------------------------------------------------------

-- Given a partially ordered set (P, <=), we can define a category whose
-- objects are the elements of P, and there is a morphism between elements
-- a and b  iff  a <= b

-- The transitivity of <= guarantees the existence of the composition law,
-- this is:
--   if f and g exist, with  f : a -> b  ,  g : b -> c
--   ==> a <= b  and  b <= c
--   ==> a <= c   (transitivity)
--   ==> exists h : a -> c
--         (so we can asign g.f = h)




------------------------------------------------------------------------
--           SECOND EXERCISE OF THE HASK CATEGORY SECTION             --
------------------------------------------------------------------------

-- Maybe functor:
{-
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)
-}

-- fmap id Nothing == Nothing 
-- fmap id (Just x) = Just (id x) == (Just x)

-- fmap (g.f) Nothing 
--   = Nothing
--   == fmap g Nothing
--   = fmap g (fmap f Nothing)

-- fmap (g.f) (Just x)
--   = Just ( g (f x) )
--   == fmap g (Just (f x))
--   = fmap g (fmap f (Just x))


-- List functor:
{-
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map
-}
{-
map _ []     = []
map f (x:xs) = f x : map f xs
-}

-- map id [] == []
-- map id (x:xs) 
--   = (id x) : map id xs
--   = x : map id xs
--   == (x:xs)

-- map (g.f) [] 
--   == []
--   = map g []
--   = map g (map f [])

-- map (g.f) (x:xs)
--   = (g (f x)) : map (g.f) xs
--   == map g ((f x):xs)
--   = map g (map f (x:xs))




------------------------------------------------------------------------
--            THIRD EXERCISE OF THE HASK CATEGORY SECTION             --
------------------------------------------------------------------------
{-
join x = x >>= id
-}

-- Maybe monad:
{-
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
-}
{- occurs that
join :: Maybe (Maybe a) -> Maybe a
join Nothing         = Nothing
join (Just Nothing)  = Nothing
join (Just (Just x)) = Just x
-}

-- (join . fmap join) Nothing
--   = join Nothing
--   == Nothing
--   = join (join Nothing)

-- (join . fmap join) (Just Nothing)
--   == Nothing
--   = (join . join) (Just Nothing)

-- (join . fmap join) (Just (Just Nothing))
--   = join (Just (join (Just Nothing)))
--   = join (Just Nothing)
--   == Nothing
--   = join (Just Nothing)
--   = (join . join) (Just (Just Nothing))

-- (join . fmap join) (Just (Just (Just x)))
--   = join (Just (join (Just (Just x))))
--   = join (Just (Just x))
--   = Just x
--   == join (Just (Just x))
--   = (join . join) (Just (Just (Just x)))


-- List monad:
--   Some examples

{-
Prelude Control.Monad> (join . fmap join) [[]]
[]
(0.00 secs, 0 bytes)
Prelude Control.Monad> (join . fmap join) []
[]
(0.00 secs, 2959128 bytes)
Prelude Control.Monad> (join . fmap join) [[]]
[]
(0.00 secs, 0 bytes)
Prelude Control.Monad> (join . fmap join) [[[]]]
[]
(0.00 secs, 3707032 bytes)
Prelude Control.Monad> (join . fmap join) [[[[]]]]
[[]]
(0.02 secs, 0 bytes)
Prelude Control.Monad> (join . join) []
[]
(0.00 secs, 0 bytes)
Prelude Control.Monad> (join . join) [[]]
[]
(0.00 secs, 0 bytes)
Prelude Control.Monad> (join . join) [[[]]]
[]
(0.00 secs, 0 bytes)
Prelude Control.Monad> (join . join) [[[[]]]]
[[]]
(0.00 secs, 0 bytes)
-}
-- Prelude Control.Monad> fmap join [ [[1,2],[3]] , [[4,5],[6,7]] ]
-- [[1,2,3],[4,5,6,7]]     <-- CHECK THIS
-- (0.00 secs, 0 bytes)
-- Prelude Control.Monad> (join . fmap join) [ [[1,2],[3]] , [[4,5],[6,7]] ]
-- [1,2,3,4,5,6,7]
-- (0.00 secs, 0 bytes)
-- Prelude Control.Monad>  join [ [[1,2],[3]] , [[4,5],[6,7]] ]
-- [[1,2],[3],[4,5],[6,7]] <- AND THIS
-- (0.00 secs, 0 bytes)
-- Prelude Control.Monad> (join . join) [ [[1,2],[3]] , [[4,5],[6,7]] ]
-- [1,2,3,4,5,6,7]
-- (0.02 secs, 0 bytes)




------------------------------------------------------------------------
--            FOURTH EXERCISE OF THE HASK CATEGORY SECTION            --
------------------------------------------------------------------------

-- Second law:
--   join . fmap return = join . return = id

-- (join . fmap return) Nothing 
--   = join Nothing
--   = Nothing
--   == join (Just Nothing)
--   = (join . return) Nothing

-- (join . fmap return) (Just x)
--   = join (Just (Just x))
--   = Just x
--   == join (Just (Just x))
--   = (join . return) (Just x)




------------------------------------------------------------------------
--            FIFTH EXERCISE OF THE HASK CATEGORY SECTION             --
------------------------------------------------------------------------

-- Third law:
--   return . f = fmap f . return

-- f :: a -> b        , (return . f)      = a -> m b
-- return :: a -> m a , (fmap f . return) = a -> m b

-- States that applying a function to a value and then
-- embedding the result into the monad is the same as
-- embedding the value into the monad and then mapping the function.



-- Fourth law:
--   join . fmap (fmap f) = fmap f . join

-- (fmap f) :: m a -> m b ,
-- fmap (fmap f) :: m (m a) -> m (m b),
-- join . fmap (fmap f) :: m (m a) -> m b
-- AND on the other side:
-- join :: m (m a) -> m a ,
-- fmap f . join :: m (m a) -> m b

-- States that, when having a 2 layer monadic value m (m a)
-- its the same joining and then mapping or 
-- mapping to the inner layer and joining afterwards.




------------------------------------------------------------------------
--            SIXTH EXERCISE OF THE HASK CATEGORY SECTION             --
------------------------------------------------------------------------

-- Recalling:
--   join m   =  m >>= id
--   fmap f m =  m >>= return . f



-- First Law:
--   join . fmap join == join . join  ??

-- (join . fmap join) m
--   = join (fmap join m)
--   = join (m >>= return . join)
--   = (m >>= return . join) >>= id 
--   = (m >>= id) >>= id     *****
--   = join (m >>= id)
--   = join (join m)
--   = (join . join) m

-- ***** OBS: we need to prove that return.join == id
--            (which is the second law)


-- Second Law:
--   join . fmap return == join . return == id ??

-- (join . fmap return) m
--   = join (fmap return m)
--   = join (m >>= return . return)
--   = join (return m)

-- (join . return) m
--   = join (return m)
--   = return m >>= id
--   = id m
--   == m


-- Third Law:
--   return . f = fmap f . return

-- (return  . f) x
--   = return (f x)

-- (fmap f . return) x
--   = fmap f (return x)
--   = (return x) >>= return . f
--   = (return . f) x


-- Fourth Law:
--   join . fmap (fmap f) = fmap f . join

-- (fmap f . join) m
--   = fmap f (join m)
--   = fmap f (m >>= id)
--   = (m >>= id) >>= return . f
--   = (m >>= id) >>= fmap f . return

--   = m >>= (\m' -> id m' >>= return . f)
--   = m >>= (\m' -> m' >>= return . f)

-- (join . fmap (fmap f)) m
--   = join (fmap (fmap f) m)
--   = join (m >>= return . (fmap f))
--   = join (m >>= \x -> return (fmap f x))
--   = (m >>= \x -> return (fmap f x)) >>= id


