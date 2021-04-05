{- class Functor f where 

The Functor class is used for types that can be mapped over. Instances of Functor should satisfy the following laws:
fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g

The instances of Functor for lists, Maybe and IO satisfy these laws.


Minimal complete definition

fmap


Methods

fmap :: (a -> b) -> f a -> f b 

(<$) :: a -> f b -> f a infixl 4


Replace all locations in the input with the same value. The default definition is fmap . const but this may be overridden with a more efficient version.
-}


{- (<$) no se importa automaticamente al abrir WinGHCi
ademas
fmap (const 'a') [1..10] hace lo mismo que
(fmap.const) 'a' [1..10]
-}








--data Booleanos = Verdad | Mentira

--instance Functor Booleanos where
--   fmap g Verdad = g Verdad
--   fmap g Mentira = g Mentira

--esto da un kind error porque 
--Main> :kind Functor
--Functor :: (* -> *) -> Constraint
--pero
--Main> :kind Booleanos
--Booleanos :: *


instance Monad Int where







data Maybe' a = Nothing' | Just' a
   deriving Show


instance  Functor Maybe'  where
    fmap f Nothing'    =  Nothing'
    fmap f (Just' x)   =  Just' (f x)


--esto funciona perfectamente porque 
--Main> :kind Maybe'
--Maybe' :: * -> *









data Weird a b = First a
               | Second b
               | Third [(a,b)]
               | Fourth (Weird a b)
   deriving (Eq, Show)


--instance Functor (Weird a b) where
--   fmap :: (a -> b) -> (Weird a b) -> (Weird a b)
--   fmap g w = case w of
--      First a -> First (g a)


--esto no funciona porque
--Main> :kind Weird
--Weird :: * -> * -> *
--y no cuadra con 
--Main> :kind Functor
--Functor :: (* -> *) -> Constraint
--pero el mensaje de error de Haskell es:
--  practicandoFunctor.hs:52:19:
--    The first argument of ‘Functor’ should have kind ‘* -> *’,
--      but ‘Weird a b’ has kind ‘*’
--    In the instance declaration for ‘Functor (Weird a b)’





data Pila a = PilaVacia | Apilar a (Pila a)


instance Show a => Show (Pila a) where
   show PilaVacia = "Culo"
   show (Apilar x p) = show x ++ " " ++ show p

--instance Functor Pila where
--   fmap f PilaVacia    = PilaVacia
--   fmap f (Apilar x p) = Apilar (f x) (fmap f p)

instance Functor Pila where
   fmap f _ = PilaVacia
   
   

crearPila :: [a] -> Pila a
crearPila = foldr (\ x p -> Apilar x p) PilaVacia 























