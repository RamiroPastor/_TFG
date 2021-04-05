{-# LANGUAGE OverloadedStrings #-}


module TFG_anexoClave where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath


import TFG_categorias
import TFG_monadasHaskell (appClass)
import TFG_System_Random hiding (format)

-------- SIMBOLOS U OTRO CODIGO --------


format :: LaTeXC l => String -> l
format = verbatim . pack


formatSym :: LaTeXC l => l -> l
formatSym = scriptsize . fbox . math


c :: Char
c = toEnum 34


----------------------------------------



resumenClave :: LaTeXC l => l
resumenClave =
  chapter "Appendix: Summary of functions "
  <> "Everything has been taken from the Haskell documentation"
  <> section "Functor context" <> funcionesFunctor <> comentariosFunctor
  <> newpage
  <> section "Applicative context" <> introApplicative <> funcionesApplicative <> comentariosApplicative
  <> newpage
  <> section "Monad context" <> funcionesMonad
  <> newpage 
  <> section "Alternative context" <> funcionesAlternative
  <> newpage
  <> section "Module System.Random" <> sysRandModule
  <> newpage
  <> section "Module Control.Monad" <> controlMonad <> namingConventions




funcionesFunctor :: LaTeXC l => l
funcionesFunctor = format t where
  t =
    "class Functor f where                                                        \n"
    <> "  The Functor class is used for types that can be mapped over.          \n  \n"
    <> "  Instances of Functor should satisfy the following laws:                 \n"
    <> "    fmap id  ==  id                                                       \n"
    <> "    fmap (f . g)  ==  fmap f . fmap g                                   \n  \n"
    <> "  Minimal complete definition                                             \n"
    <> "    fmap                                                                \n  \n"
    <> "  Methods                                                                 \n"
    <> "    fmap :: (a -> b) -> f a -> f b                                        \n"
    <> "    (<$) :: a -> f b -> f a                             infixl 4        \n  \n"
    <> "  Predefined functions  (in Data.Functor)                                 \n"
    <> "    (<$>) :: Functor f => (a -> b) -> f a -> f b        infixl 4          \n"
    <> "    (<$>) = fmap                                                          \n"
    <> "    ($>) :: Functor f => f a -> b -> f b                infixl 4          \n"
    <> "    ($>) = flip (<$)                                                      \n"
    <> "    void :: Functor f => f a -> f ()                                      \n"
    <> "    void x = () <$ x                                                      \n"



comentariosFunctor :: LaTeXC l => l
comentariosFunctor =
  indent <> formatSym "(<$>)" <> " is an infix synonym for " <> fbox "fmap" <> "."
  <> par <> "The method " <> formatSym "(<$)" <> " replaces all locations in the input with the same value. The default definition is " <> fbox "fmap . const" <> ", but this may be overriden with a more efficient version."
  <> par <> formatSym "($>)" <> " is a flipped version of " <> formatSym "(<$)" <> "."
  <> par <> fbox "void" <> " value discards or ignores the result of evaluation, such as the return value of an " <> fbox "System.IO.IO" <> " action."




funcionesApplicative :: LaTeXC l => l
funcionesApplicative = format t where
  t =  
    "class Functor f => Applicative f where                                             \n" 
    <> "  A functor with application, providing operations to                           \n"
    <> "  embed pure expressions (pure), and                                            \n"
    <> "  sequence computations and combine their results: (<*>).                     \n  \n"  
    <> "  Instances of Functor should satisfy the following laws:                       \n"
    <> "    pure id <*> v = v                                  -- identity              \n"
    <> "    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)       -- composition           \n"
    <> "    pure f <*> pure x = pure (f x)                     -- homomorphism          \n"
    <> "    u <*> pure y = pure ($ y) <*> u                    -- interchange         \n  \n"
    <> "    As a consequence of these laws, the Functor instance for f will satisfy     \n"
    <> "      fmap f x = pure f <*> x                                                 \n  \n"
    <> "    If f is also a Monad, it should satisfy                                     \n"
    <> "      pure = return                                                             \n"
    <> "      (<*>) = ap                                                                \n"
    <> "      (which implies that pure and <*> satisfy the applicative functor laws). \n  \n"
    <> "  Minimal complete definition                                                   \n"       
    <> "    pure, (<*>)                                                               \n  \n"
    <> "  Methods                                                                       \n"
    <> "    pure :: a -> f a                                                            \n"
    <> "    (<*>) :: f (a -> b) -> f a -> f b                     infixl 4              \n"
    <> "    (*>) :: f a -> f b -> f b                             infixl                \n"
    <> "    u *> v = pure (const id) <*> u <*> v                                        \n"
    <> "    (<*) :: f a -> f b -> f a                             infixl 4              \n"
    <> "    u <* v = pure const <*> u <*> v                                           \n  \n"
    <> "  Utility functions                                                             \n"
    <> "    (<**>) :: Applicative f => f a -> f (a -> b) -> f b   infixl 4              \n"
    <> "    (<**>) = liftA2 (flip ($))                                                  \n"
    <> "    liftA  :: Applicative f => (a -> b) -> f a -> f b                            \n"
    <> "    liftA f a = pure f <*> a                                                    \n"
    <> "    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c               \n"
    <> "    liftA2 f a b = fmap f a <*> b                                               \n"
    <> "    liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d   \n"
    <> "    liftA3 f a b c = fmap f a <*> b <*> c                                       \n"
    <> "    when   :: (Applicative f) => Bool -> f () -> f ()                           \n"
    <> "    when p s = if p then s else pure ()                                         \n"



comentariosApplicative :: LaTeXC l => l
comentariosApplicative =
  par <> formatSym "(*>)" <> " and " <> formatSym "(<*)" <> " are already defined, but may be overriden with equivalent specialized implementations."
  <> par <> formatSym "(<**>)" <> " is a variant of " <> formatSym "(<*>)" <> " with the arguments reversed."
  <> par <> fbox "liftA" <> " lifts a function to actions. This function may be used as a value for " <> fbox "fmap" <> " in a " <> fbox "Functor" <> " instance."
  <> par <> fbox "liftA2" <> " lifts a binary function to actions."
  <> par <> fbox "liftA3" <> " lifts a ternary function to actions."
  <> par <> fbox "when" <> " is a conditional execution of " <> fbox "Applicative" <> " expressions."



introApplicative :: LaTeXC l => l
introApplicative =
  "The" <> fbox "Control.Applicative" <> " module describes a structure intermediate between a functor and a monad (technically, a strong lax monoidal functor)"
  <> ". Compared with monads, this interface lacks the full power of the binding operation " <> formatSym "(>>=)" <> ", but "
  <> enumerate (item Nothing <> prop1 <> item Nothing <> prop2 <> item Nothing <> prop3)
    where
      prop1 = "it has more instances."
      prop2 = "it is sufficient for many uses, e.g. context-free parsing, or the Traversable class."
      prop3 = "instances can perform analysis of computations before they are executed, and thus produce shared optimizations."




funcionesAlternative :: LaTeXC l => l
funcionesAlternative = format t where
  t =
    "class Applicative f => Alternative f where                                          \n"
    <> "  A monoid on applicative functors.                                            \n  \n"
    <> "  If defined, some and many should be the least solutions of the equations:      \n"
    <> "    some v = (:) <$> v <*> many v                                                \n"
    <> "    many v = some v <|> pure []                                                \n  \n"
    <> "  Minimal complete definition                                                    \n"
    <> "    empty, (<|>)                                                                 \n"
    <> "  Methods                                                                        \n"
    <> "    empty :: f a                                                                 \n"
    <> "      The identity of <|>                                                        \n"
    <> "    (<|>) :: f a -> f a -> f a                            infixl 3               \n"
    <> "      An associative binary operation                                            \n"
    <> "    some :: f a -> f [a]                                                         \n"
    <> "      One or more                                                                \n"
    <> "    many :: f a -> f [a]                                                         \n"
    <> "      Zero or more.                                                            \n  \n"
    <> "  Utility functions                                                              \n"
    <> "    optional :: Alternative f => f a -> f (Maybe a)                              \n"
    <> "      One or none.                                                               \n"




introMonad :: LaTeXC l => l
introMonad = 
  "The " <> fbox "Monad" <> " class defines the basic operations over a " <> textit "monad" <> ", a concept from a branch of mathematics known as " <> textit "category theory"
  <> ". From the perspective of a Haskell programmer, however, it is best to think of a monad as an " <> textit "abstract datatype" <> " of actions. Haskell's " <> fbox "do" <> " expressions provide a convenient syntax for writing monadic expressions."



funcionesMonad :: LaTeXC l => l
funcionesMonad = format t where
  t =
    "class Applicative m => Monad m where                                               \n  \n"
    <> "  Instances of Monad should satisfy the following laws:                           \n"
    <> "    return a >>= k  =  k a                                                        \n"
    <> "    m >>= return  =  m                                                            \n"
    <> "    m >>= (x -> k x >>= h)  =  (m >>= k) >>= h                                  \n  \n"
    <> "  Furthermore, the Monad and Applicative operations should relate as follows:     \n"
    <> "    pure = return                                                                 \n"
    <> "    (<*>) = ap                                                                    \n"
    <> "  The above laws imply:                                                           \n"
    <> "    fmap f xs  =  xs >>= return . f                                               \n"
    <> "    (>>) = (*>)                                                                   \n"
    <> "    pure and (<*>) satisfy the applicative functor laws.                        \n  \n"
    <> "  Minimal complete definition                                                     \n"
    <> "    (>>=)                                                                       \n  \n"
    <> "  Methods                                                                         \n"
    <> "    (>>=)    :: m a -> (a -> m b) -> m b                     infixl 1               \n"
    <> "    (>>)     :: m a -> m b -> m b                            infixl 1               \n"
    <> "    m >> k   =   m >>= \\_ -> k                                                   \n"
    <> "    return   :: a -> m a                                                            \n"
    <> "    return   =   pure                                                             \n"
    <> "    fail     :: String -> m a                                                       \n" 
    <> "    fail s   =   error s                                                        \n  \n"
    <> "  Utility functions                                                               \n"
    <> "    join     :: (Monad m) => m (m a) -> m a                                         \n"
    <> "    join x            =  x >>= id                                                 \n"
    <> "    (=<<)    :: Monad m => (a -> m b) -> m a -> m b                                 \n"
    <> "    f =<< x    =    x >>= f                                                       \n"
    <> "    sequence :: Monad m => [m a] -> m [a]                                         \n"
    <> "    sequence = mapM id                                                            \n"
    <> "    mapM     :: Monad m => (a -> m b) -> [a] -> m [b]                               \n"
    <> "    mapM f as = foldr k (return []) as                                            \n"         
    <> "    where                                                                         \n"
    <> "      k a r = do { x <- f a; xs <- r; return (x:xs) }                             \n"
    <> "    liftM    :: (Monad m) => (a1 -> r) -> m a1 -> m r                               \n"
    <> "    liftM f m1         = do { x1 <- m1; return (f x1) }                           \n"
    <> "    liftM2   :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r                 \n"
    <> "    liftM2 f m1 m2     = do { x1 <- m1; x2 <- m2; return (f x1 x2) }              \n"
    <> "    liftM3   :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r   \n"
    <> "    liftM3 f m1 m2 m3  = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) } \n"
    <> "    ap       :: (Monad m) => m (a -> b) -> m a -> m b                               \n"
    <> "    ap m1 m2           = do { x1 <- m1; x2 <- m2; return (x1 x2) }                 \n"



sysRandModule :: LaTeXC l => l
sysRandModule =
  randomGenCode <> stdGenCode <> randomClassCode 
  <> newpage
  <> "And the global random number generator"
  <> format "  getStdRandom :: (StdGen -> (a, StdGen)) -> IO a   \n"
  <> par <> "Uses the supplied function to get a value from the current global random generator, and updates the global generator with the new generator returned by the function."
  <> format "  getStdGen :: IO StdGen    \n"
  <> par <> "Gets the global random number generator."
  <> format "  setStdGen :: StdGen -> IO ()  \n"
  <> par <> "Sets the global random number generator."
  <> format "  newStdGen :: IO StdGen       \n"
  <> par <> "Applies " <> fbox "split" <> " to the current global random generator, updates it with one of the results, and returns the other." 




controlMonad :: LaTeXC l => l
controlMonad = 
  "The Functor, Monad and MonadPlus classes, with some useful operations on monads. Some of the information is already exposed in the previous sections."
  <> format t where
    t =
      "class Functor f where                                                            \n"
      <> "  already seen                                                              \n  \n"
      <> "class Applicative m => Monad m where                                          \n"
      <> "  already seen                                                              \n  \n"
      <> "class (Alternative m, Monad m) => MonadPlus m where                           \n"
      <> "  Monads that also support choice and failure.                              \n  \n"
      <> "  Instances of MonadPlus should satisfy the following laws:                   \n"
      <> "    mzero `mplus` m = m                                                       \n"
      <> "    m `mplus` mzero = m                                                       \n"
      <> "    associativity of mplus                                                    \n"
      <> "    mzero >>= f     = mzero                                                   \n"
      <> "    m >> mzero      = mzero                                                 \n  \n"
      <> "  Minimal complete definition                                                 \n"
      <> "    Nothing                                                                   \n"
      <> "  Methods                                                                     \n"
      <> "    mzero :: m a                                                              \n"
      <> "    mplus :: m a -> m a -> m a                                              \n  \n"
      <> "Basic Monad functions                                                         \n"
      <> "  mapM      :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)       \n"
      <> "  mapM_     :: (Foldable t, Monad m)    => (a -> m b) -> t a -> m ()          \n"
      <> "  forM      :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)       \n"
      <> "  forM_     :: (Foldable t, Monad m)    => t a -> (a -> m b) -> m ()          \n"
      <> "  sequence  :: (Traversable t, Monad m) => t (m a) -> m (t a)                 \n"
      <> "  sequence_ :: (Foldable t, Monad m)    => t (m a) -> m ()                    \n"
      <> "  (=<<)     :: Monad m   => (a -> m b) -> m a -> m b                infixr 1  \n"
      <> "  (>=>)     :: Monad m   => (a -> m b) -> (b -> m c) -> a -> m c    infixr 1  \n"
      <> "  (<=<)     :: Monad m   => (b -> m c) -> (a -> m b) -> a -> m c    infixr 1  \n"
      <> "  forever   :: Monad m   => m a -> m b                                        \n"
      <> "  void      :: Functor f => f a -> f ()                                     \n  \n"
      <> "Generalisations of list functions                                             \n"
      <> "  join      :: Monad m => m (m a) -> m a                                      \n"
      <> "  msum      :: (Foldable t, MonadPlus m) => t (m a) -> m a                    \n"
      <> "  mfilter      :: MonadPlus m => (a -> Bool) -> m a -> m a                    \n"
      <> "  filterM      :: Monad m => (a -> m Bool) -> [a] -> m [a]                    \n"
      <> "  mapAndUnzipM :: Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])           \n"
      <> "  zipWithM     :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]           \n"
      <> "  zipWithM_    :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()            \n"
      <> "  foldM     :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b    \n"
      <> "  foldM_    :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()   \n"
      <> "  replicateM   :: Monad m => Int -> m a -> m [a]                              \n"
      <> "  replicateM_  :: Monad m => Int -> m a -> m ()                             \n  \n"
      <> "Conditional execution of monadic expressions                                  \n"
      <> "  guard     :: Alternative f => Bool -> f ()                                  \n"
      <> "  when      :: Applicative f => Bool -> f () -> f ()                          \n"
      <> "  unless    :: Applicative f => Bool -> f () -> f ()                        \n  \n"
      <> "Monadic lifting operators                                                     \n"
      <> "  liftM  :: Monad m => (a1 -> r) -> m a1 -> m r                               \n"
      <> "  liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r                 \n"
      <> "  liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r   \n"
      <> "  liftM4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r                  \n"
      <> "  liftM5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r    \n"
      <> "  ap :: Monad m => m (a -> b) -> m a -> m b                                 \n  \n"
      <> "Strict monadic functions                                                      \n"
      <> "  (<$!>)    :: Monad m => (a -> b) -> m a -> m b                    infixl 4  \n"
 
 


namingConventions :: LaTeXC l => l
namingConventions =
  paragraph "Naming conventions" <> "The functions in this library use the following naming conventions:"
  <> itemize (aux1 <> aux2 <> aux3)
  where
    aux1  = item Nothing <> "A postfix `M' always stands for a function in the Kleisli category: The monad type constructor " <> fbox "m" <> " is added to function results (modulo currying) and nowhere else. So, for example," <> format aux1'
    aux1' = "  filter  ::              (a ->   Bool) -> [a] ->   [a]  \n" <> "  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]   \n"
    aux2  = item Nothing <> "A postfix `_' changes the result type from " <> fbox "m a" <> " to " <> fbox "m ()" <> ". Thus, for example:" <> format aux2'
    aux2' = "  sequence  :: Monad m => [m a] -> m [a]   \n" <> "  sequence_ :: Monad m => [m a] -> m ()   \n"
    aux3  = item Nothing <> "A prefix `m' generalizes an existing function to a monadic form. Thus, for example:" <> format aux3'
    aux3' = "  sum  :: Num a       => [a]   -> a   \n" <> "  msum :: MonadPlus m => [m a] -> m a   \n"

   



{-

"EQUIVALENCIAS A SUCIO (si se verifican las leyes exigidas)

fmap f x   =   (pure f <*> x)   =   liftA f x   =   (xs >>= return . f)


u *> v   =   pure (const id) <*> u <*> v   =   u *> v = (id <$ u) <*> v   =   liftA2 (const id) u v   =   (u >> v) 
  (<*)   =   liftA2 const                     


  pure   =   return
 (<*>)   =   ap



COSAS INTERESANTES

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x) 
    (u, f) <*> (v, x) = (u `mappend` v, f x)


-}


{-
DE ALTERNATIVE



Instances
Alternative [] Source    
Alternative Maybe Source    
Alternative ReadP Source    
Alternative ReadPrec Source    
Alternative STM Source    
ArrowPlus a => Alternative (ArrowMonad a) Source    
MonadPlus m => Alternative (WrappedMonad m) Source    
Alternative f => Alternative (Alt * f) Source    
(ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) Source    


Instances


newtype Const a b Source
Constructors
Const   
getConst :: a
 

Instances

Bifunctor Const Source    
Functor (Const m) Source    
Monoid m => Applicative (Const m) Source    
Foldable (Const m) Source    
Traversable (Const m) Source    
Generic1 (Const a) Source    
Eq a => Eq (Const a b) Source    
Ord a => Ord (Const a b) Source    
Read a => Read (Const a b) Source    
Show a => Show (Const a b) Source    
Generic (Const a b) Source    
Monoid a => Monoid (Const a b) Source    
type Rep1 (Const a) Source    
type Rep (Const a b) Source    


newtype WrappedMonad m a Source
Constructors
WrapMonad   
unwrapMonad :: m a
Instances
Monad m => Monad (WrappedMonad m) Source    
Monad m => Functor (WrappedMonad m) Source    
Monad m => Applicative (WrappedMonad m) Source    
Generic1 (WrappedMonad m) Source    
MonadPlus m => Alternative (WrappedMonad m) Source    
Generic (WrappedMonad m a) Source    
type Rep1 (WrappedMonad m) Source    
type Rep (WrappedMonad m a) Source  

  
newtype WrappedArrow a b c Source
Constructors
WrapArrow   
unwrapArrow :: a b c
Instances
Arrow a => Functor (WrappedArrow a b) Source    
Arrow a => Applicative (WrappedArrow a b) Source    
Generic1 (WrappedArrow a b) Source    
(ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) Source    
Generic (WrappedArrow a b c) Source    
type Rep1 (WrappedArrow a b) Source    
type Rep (WrappedArrow a b c) Source    


newtype ZipList a Source
Lists, but with an Applicative functor based on zipping, so that
f <$> ZipList xs1 <*> ... <*> ZipList xsn = ZipList (zipWithn f xs1 ... xsn)
Constructors
ZipList   
getZipList :: [a]
Instances
Functor ZipList Source    
Applicative ZipList Source    
Generic1 ZipList Source    
Eq a => Eq (ZipList a) Source    
Ord a => Ord (ZipList a) Source    
Read a => Read (ZipList a) Source    
Show a => Show (ZipList a) Source    
Generic (ZipList a) Source    
type Rep1 ZipList   
type Rep (ZipList a)     

-}