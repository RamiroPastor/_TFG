{-# LANGUAGE OverloadedStrings #-}

module TFG_monadTrans where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath

import TFG_maybeMonad (bind)
import TFG_tablaMonadas
import TFG_comandoChangeMargin





-------- SIMBOLOS U OTRO CODIGO --------

c :: Char
c = toEnum 34

format :: LaTeXC c => String -> c
format = verbatim . pack

passphrase :: LaTeXC l => l
passphrase = format t where
  t =
    "  getPassphrase :: IO (Maybe String)                              \n"
    <> "  getPassphrase = do s <- getLine                              \n"
    <> "                     if isValid s then return $ Just s         \n"
    <> "                                  else return Nothing      \n  \n"
    <> "  -- The validation test could be anything we want it to be.   \n"
    <> "  isValid :: String -> Bool                                    \n"
    <> "  isValid s = length s >= 8                                    \n"
    <> "              && any isAlpha s                                 \n"
    <> "              && any isNumber s                                \n"
    <> "              && any isPunctuation s                           \n"

askPassphrase :: LaTeXC l => l
askPassphrase = format t where
  t =
    "  askPassphrase :: IO ()                                                         \n"
    <> "  askPassphrase = do putStrLn "<>[c]<>"Insert your new passphrase:"<>[c]<>"                   \n"
    <> "                     maybe_value <- getPassphrase                             \n"
    <> "                     if isJust maybe_value                                    \n"
    <> "                       then do putStrLn "<>[c]<>"Storing in database..."<>[c]<>" -- do stuff  \n"
    <> "                       else putStrLn "<>[c]<>"Passphrase invalid."<>[c]<>"                    \n"


maybeTbind :: LaTeXC l => l
maybeTbind = format t where
  t =
    "  -- The signature of (>>=), specialized to MaybeT m                 \n"
    <> "  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b      \n  \n"
    <> "  x >>= f = MaybeT $ do maybe_value <- runMaybeT x                \n"
    <> "                        case maybe_value of                       \n"
    <> "                             Nothing    -> return Nothing         \n"
    <> "                             Just value -> runMaybeT $ f value    \n"

maybeTbind_exposed :: LaTeXC l => l
maybeTbind_exposed =
  item Nothing <> "First, the " <> fbox "runMaybeT" <> " accessor unwraps " <> fbox "x" <> " into an " <> fbox "m (Maybe a)" <> " computation. That shows us that the whole " <> fbox "do" <> " block is in " <> fbox "m" <> "."
  <> item Nothing <> "Still in the first line, " <> fbox (math "<-") <> " extracts a " <> fbox "Maybe a" <> " value from the unwrapped computation."
  <> item Nothing <> "The " <> fbox "case" <> " statement tests " <> fbox "maybe_value" <> ":"
  <> itemize (aux1 <> aux2)
  <> item Nothing <> "Finally, the " <> fbox "do" <> " block as a whole has " <> fbox "m (Maybe b)" <> " type; so it is wrapped with the " <> fbox "MaybeT" <> " constructor."
    where
      aux1 = item Nothing <> "With " <> fbox "Nothing" <> ", we return " <> fbox "Nothing" <> " into " <> fbox "m" <> ";"
      aux2 = item Nothing <> "With " <> fbox "Just" <> ", we apply " <> fbox "f" <> " to the " <> fbox "value" <> " from the " <> fbox "Just" <> ". Since " <> fbox "f" <> " has " <> fbox "MaybeT m b" <> " as result type, we need an extra " <> fbox "runMaybeT" <> " to put the result back into the " <> fbox "m" <> " monad."

maybeBind :: LaTeXC l => l
maybeBind = format t where
  t =
    "  -- (>>=) for the Maybe monad                       \n"
    <> "  maybe_value >>= f = case maybe_value of         \n"
    <> "                          Nothing -> Nothing      \n"
    <> "                          Just value -> f value   \n"

instanciando :: LaTeXC l => l
instanciando = format t where
  t =
    "  instance Monad m => MonadPlus (MaybeT m) where                       \n"
    <> "      mzero     = MaybeT $ return Nothing                              \n"
    <> "      mplus x y = MaybeT $ do maybe_value <- runMaybeT x               \n"
    <> "                              case maybe_value of                      \n"
    <> "                                   Nothing    -> runMaybeT y           \n"
    <> "                                   Just _     -> return maybe_value \n \n"
    <> "  instance MonadTrans MaybeT where                                     \n"
    <> "      lift = MaybeT . (liftM Just)                                     \n"

pwRecode1 :: LaTeXC l => l
pwRecode1 = format t where
  t =
    "  getValidPassphrase :: MaybeT IO String                                      \n"
    <> "  getValidPassphrase = do s <- lift getLine                                \n"
    <> "                          guard (isValid s) -- MonadPlus provides guard.   \n"
    <> "                          return s                                      \n \n"
    <> "  askPassphrase :: MaybeT IO ()                                            \n"
    <> "  askPassphrase = do lift $ putStrLn "<>[c]<>"Insert your new passphrase:"<>[c]<>"         \n"
    <> "                     value <- getValidPassphrase                           \n"
    <> "                     lift $ putStrLn "<>[c]<>"Storing in database..."<>[c]<>"              \n"

pwRecode2 :: LaTeXC l => l
pwRecode2 = format t where
  t =
    "  askPassword :: MaybeT IO ()                                       \n"
    <> "  askPassword = do lift $ putStrLn "<>[c]<>"Insert your new password:"<>[c]<>"   \n"
    <> "                   value <- msum $ repeat getValidPassphrase     \n"
    <> "                   lift $ putStrLn "<>[c]<>"Storing in database..."<>[c]<>"      \n"

----------------------------------------




monadTrans :: LaTeXC l => l
monadTrans = 
  "We have seen how monads can help handling " <> fbox "IO" <> " actions, " <> fbox "Maybe" <> ", lists, and state. With monads providing a common way to use such useful general-purpose tools, a natural thing we might want to do is using the capabilities of "
  <> textit "several" <> " monads at once. For instance, a function could use both I/O and " <> fbox "Maybe" <> " exception handling. While a type like " <> fbox "IO (Maybe a)" <> " would work just fine, it would force us to do pattern matching within " 
  <> fbox "IO" <> " do-blocks to extract values, something that the " <> fbox "Maybe" <> " monad was meant to spare us from."
  <> par <> "Enter " <> textbf "monad transformers" <> ": special types that allow us to roll two monads into a single one that shares the behavior of both."
  <> subsection "Passphrase validation" <> contrasenyas
  <> subsection "A simple monad transformer: MaybeT" <> maybeT
  <> subsection "A plethora of transformers" <> plethoraT
  <> subsection "Lifting" <> lifting
  <> subsection "Implementing transformers" <> implementingT




contrasenyas :: LaTeXC l => l
contrasenyas =
  "Consider a real-life problem for IT staff worldwide: getting users to create strong passphrases. One approach: force the user to enter a minimum length with various irritating requirements (such as at least one capital letter, one number, one non-alphanumeric character, etc.)"
  <> par <> "Here's a Haskell function to acquire a passphrase from a user:"
  <> passphrase
  <> "First and foremost, " <> fbox "getPassphrase" <>  " is an " <> fbox "IO" <> " action, as it needs to get input from the user. We also use " <> fbox "Maybe" 
  <> ", as we intend to return " <> fbox "Nothing"  <> " in case the password does not pass the " <> fbox "isValid"
  <> ". Note, however, that we aren't actually using " <> fbox "Maybe" <> " as a monad here: the " <> fbox "do" <> " block is in the " <> fbox "IO" <> " monad, and we just happen to " <> fbox "return" <>" a " <> fbox "Maybe" <> " value into it."
  <> par <> "Monad transformers not only make it easier to write " <> fbox "getPassphrase" <> " but also simplify all the code instances. Our passphrase acquisition program could continue like this:"
  <> askPassphrase
  <> "The code uses one line to generate the " <> fbox "maybe_value" <> " variable followed by further validation of the passphrase."
  <> "With monad transformers, we will be able to extract the passphrase in one go - without any pattern matching or equivalent bureaucracy like " <> fbox "isJust" <> ". The gains for our simple example might seem small but will scale up for more complex situations."




maybeT :: LaTeXC l => l
maybeT =
  "To simplify " <> fbox "getPassphrase" <> " and all the code that uses it, we will define a " <> textit "monad transformer" <> " that gives the " <> fbox "IO" <> " monad some characteristics of the " <> fbox "Maybe" <> " monad; we will call it " <> fbox "MaybeT" 
  <> ". That follows a convention where monad transformers have a " <> qts (fbox "T") <> " appended to the name of the monad whose characteristics they provide."
  <> par <> fbox "MaybeT" <> " is a wrapper around " <> fbox "m (Maybe a)" <> ", where " <> fbox "m" <> " can be any monad (" <> fbox "IO" <> " in our example):"
  <> format "  newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }    \n"
  <> "This data type definition specifies a " <> fbox "MaybeT" <> " type constructor, parameterized over " <> fbox "m" <> ", with a term constructor, also called " <> fbox "MaybeT" <> ", and a convenient accessor function " <> fbox "runMaybeT" <> ", with which we can access the underlying representation."
  <> par <> "The whole point of monad transformers is that " <> textit "they are monads themselves" <> "; and so we need to make " <> fbox "MaybeT m" <> " an instance of the " <> fbox "Monad" <> " class:"
  <> format "  instance Monad m => Monad (MaybeT m) where   \n      return  = MaybeT . return . Just   \n"
  <> "It would also have been possible (though arguably less readable) to write " <> fbox "return = MaybeT . return . return" <> "."
  <> par <> "As in all monads, the bind operator is the heart of the transformer."
  <> maybeTbind
  <> "Starting from the first line of the " <> fbox "do" <> " block:"
  <> enumerate maybeTbind_exposed
  <> par <> "It may look a bit complicated; but aside from the copious amounts of wrapping and unwrapping, the implementation does the same as the familiar bind operator of " <> fbox "Maybe" <> ":"
  <> maybeBind
  <> "Why use the " <> fbox "MaybeT" <> " constructor before the " <> fbox "do" <> " block while we have the accessor " <> fbox "runMaybeT" <> " within " <> fbox "do" <> "? Well, the " 
  <> fbox "do" <> " block must be in the " <> fbox "m" <> " monad, not in " <> fbox "MaybeT m" <> " (which lacks a defined bind operator at this point)."
  <> paragraph "Note" <> "The chained functions in the definition of " <> fbox "return" <> " suggest a metaphor, which you may find either useful or confusing. Consider the combined monad as a " <> textit "sandwich"
  <> ". This metaphor might suggest three layers of monads in action, but there are only two really: the inner monad and the combined monad (there are no binds or returns done in the base monad; it only appears as part of the implementation of the transformer)"
  <> ". If you like this metaphor at all, think of the transformer and the base monad as two parts of the same thing - the " <> textit "bread" <> " - which wraps the inner monad."
  <> par <> "Technically, this is all we need; however, it is convenient to make MaybeT an instance of a few other classes:"
  <> instanciando
  <> par <> fbox "MonadTrans" <> " implements the " <> fbox "lift" <> " function, so we can take functions from the " <> fbox "m" <> " monad and bring them into the " <> fbox "MaybeT m" <> " monad in order to use them in " 
  <> fbox "do" <> " blocks. As for " <> fbox "MonadPlus" <> ", since " <> fbox "Maybe" <> " is an instance of that class it makes sense to make the " <> fbox "MaybeT" <> " an instance too."
  <> subsubsection "Application to the passphrase example"
  <> "With all this done, here is what the previous example of passphrase management looks like:"
  <> pwRecode2
  <> "The code is now simpler, especially in the user function " <> fbox "askPassphrase" <> ". Most importantly, we do not have to manually check whether the result is " 
  <> fbox "Nothing" <> " or " <> fbox "Just" <> ": the bind operator takes care of that for us."
  <> par <> "Note how we use " <> fbox "lift" <> " to bring the functions " <> fbox "getLine" <> " and " <> fbox "putStrLn" <> " into the " <> fbox "MaybeT IO" <> " monad. Also, since "
  <> fbox "MaybeT IO" <> " is an instance of " <> fbox "MonadPlus" <> ", checking for passphrase validity can be taken care of by a " <> fbox "guard" <> " statement, which will return " <> fbox "mzero" <> " (i.e. " <> fbox "IO Nothing" <> ") in case of a bad passphrase."
  <> "Incidentally, with the help of " <> fbox "MonadPlus" <> " it also becomes very easy to ask the user " <> textit "ad infinitum" <> " for a valid passphrase:"
  <> pwRecode2




plethoraT :: LaTeXC l => l
plethoraT =
  "The " <> fbox "transformers" <> " package provides modules with transformers for many common monads (" <> fbox "MaybeT" <> ", for instance, can be found in " <> fbox "Control.Monad.Trans.Maybe" 
  <> "). These are defined consistently with their non-transformer versions; that is, the implementation is basically the same except with the extra wrapping and unwrapping needed to thread the other monad. From this point on, we will use "
  <> textbf "base monad" <> " to refer to the non-transformer monad (e.g. Maybe in MaybeT) on which a transformer is based and " <> textbf "inner monad" <> " to refer to the other monad (e.g. IO in MaybeT IO) on which the transformer is applied."
  <> par <> "To pick an arbitrary example, " <> fbox "ReaderT Env IO String" <> " is a computation which involves reading values from some environment of type " <> fbox "Env" <> " (the semantics of " 
  <> fbox "Reader" <> ", the base monad) and performing some " <> fbox "IO" <> " in order to give a value of type " <> fbox "String" <> ". Since the " <> bind <> " operator and " <> fbox "return" <> " for the transformer mirror the semantics of the base monad, a "
  <> fbox "do" <> " block of type " <> fbox "ReaderT Env IO String" <> " will, from the outside, look a lot like a " <> fbox "do" <> " block of the " <> fbox "Reader" <> " monad, except that " <> fbox "IO" <> " actions become trivial to embed by using " <> fbox "lift" <> "."
  <> subsubsection "Type juggling"
  <> "We have seen that the type constructor for " <> fbox "MaybeT" <> " is a wrapper for a " <> "Maybe" <> " value in the inner monad. So, the corresponding accessor " <> fbox "runMaybeT" <> " gives us a value of type " 
  <> fbox "m (Maybe a)" <> " - i.e. a value of the base monad returned in the inner monad. Similarly, for the " <> fbox "ListT" <> " and " <> fbox "ExceptT" <> " transformers, which are built around lists and " <> fbox "Either" <> " respectively:"
  <> format "  runListT :: ListT m a -> m [a]  \n"
  <> " and "
  <> format "  runExceptT :: ExceptT e m a -> m (Either e a)   \n"
  <> "Not all transformers are related to their base monads in this way, however. Unlike the base monads in the two examples above, the " <> fbox "Writer" <> ", " <> fbox "Reader" <> ", " <> fbox "State" <> ", and " <> fbox "Cont" 
  <> " monads have neither multiple constructors nor constructors with multiple arguments. For that reason, they have " <> fbox "run..." <> " functions which act as simple unwrappers, analogous to the " 
  <> fbox "run...T" <> " of the transformer versions. The table below shows the result types of the " <> fbox "run..." <> " and " <> fbox "run...T" <> " functions in each case, which may be thought of as the types wrapped by the base and transformed monads respectively."
  <> footnote ( "The wrapping interpretation is only literally true for versions of the " <> fbox "mtl" <> " package older than 2.0.0.0 ." )
  <> "\n \n"
  <> center tablaTransformers
  <> "\n"
  <> par <> "Notice that the base monad is absent in the combined types. Without interesting constructors (of the sort for " <> fbox "Maybe" <> " or lists), there is no reason to retain the base monad type after unwrapping the transformed monad. It is also worth noting that in the latter three cases we have function types being wrapped. " 
  <> fbox "StateT" <> ", for instance, turns state-transforming functions of the form " <> fbox (math ("s "<>(raw "\\,")<>to<>" (a, s)" )) <> " into state-transforming functions of the form " 
  <> fbox (math ("s "<>(raw "\\,")<>to)<>"m (a, s)" ) <> "; only the result type of the wrapped function goes into the inner monad. " <> fbox "ReaderT" <> " is analogous. " <> fbox "ContT" <> " is different because of the semantics of " 
  <> fbox "Cont" <> " (the " <> textit "continuation" <> " monad): the result types of both the wrapped function and its function argument must be the same, and so the transformer puts both into the inner monad. In general, there is no magic formula to create a transformer version of a monad; the form of each transformer depends on what makes sense in the context of its non-transformer type."    




lifting :: LaTeXC l => l
lifting =
  "We will now have a more detailed look at the " <> fbox "lift" <> " function, which is critical in day-to-day use of monad transformers. The first thing to clarify is the name "
  <> qts "lift" <> ". One function with a similar name that we already know is " <> fbox "liftM" <> ". As we already know, it is a monad-specific version of " <> fbox "fmap" <> ":"
  <> format "  liftM :: Monad m => (a -> b) -> m a -> m b   \n"
  <> fbox "liftM" <> " applies a function " <> fbox (math "(a -> b)") <> " to a value within a monad " <> fbox "m" <> ". We can also look at it as a function of just one argument:"
  <> format "  liftM :: Monad m => (a -> b) -> (m a -> m b)   \n"
  <> par <> fbox "liftM" <> " converts a plain function into one that acts within " <> fbox "m" <> ". By " <> qts "lifting" <> ", we refer to bringing something into something else " <> " -- in this case, a function into a monad."
  <> par <> fbox "liftM" <> " allows us to apply a plain function to a monadic value without needing do-blocks or other such tricks:"
  <> "\n \n"
  <> center tablaPeque
  <> "\n"
  <> par <> "The " <> fbox "lift" <> " function plays an analogous role when working with monad transformers. It brings (or, to use another common word for that, " <> textit "promotes" <> ") inner monad computations to the combined monad"
  <> ". By doing so, it allows us to easily insert inner monad computations as part of a larger computation in the combined monad."
  <> par <> fbox "lift" <> " is the single method of the " <> fbox "MonadTrans" <> " class, found in " <> fbox "Control.Monad.Trans.Class" <> ". All monad transformers are instances of " 
  <> fbox "MonadTrans" <> ", and so " <> fbox "lift" <> " is available for them all."
  <> format "  class MonadTrans t where   \n      lift :: (Monad m) => m a -> t m a   \n"
  <> "There is a variant of " <> fbox "lift" <> " specific to " <> fbox "IO" <> " operations, called " <> fbox "liftIO" <> ", which is the single method of the " <> fbox "MonadIO" <> " class in " <> fbox "Control.Monad.IO.Class" <> "."
  <> format "  class (Monad m) => MonadIO m where   \n     liftIO :: IO a -> m a   \n"
  <> par <> fbox "liftIO" <> " can be convenient when multiple transformers are stacked into a single combined monad. In such cases, " <> fbox "IO" <> " is always the innermost monad, and so we typically need more than one lift to bring " 
  <> fbox "IO" <> " values to the top of the stack. " <> fbox "liftIO" <> " is defined for the instances in a way that allows us to bring an " <> fbox "IO" <> " value from any depth while writing the function a single time."
  <> subsubsection "Implementing lift"
  <> "Implementing " <> fbox "lift" <> " is usually pretty straightforward. Consider the " <> fbox "MaybeT" <> " transformer: "
  <> format "  instance MonadTrans MaybeT where   \n      lift m = MaybeT (liftM Just m)   \n"
  <> "We begin with a monadic value of the inner monad. With " <> fbox "liftM" <> " (" <> fbox "fmap" <> " would have worked just as fine), we slip the base monad (through the " <> fbox "Just" <> " constructor) underneath, so that we go from " 
  <> fbox "m a" <> " to " <> fbox "m (Maybe a)" <> "). Finally, we use the " <> fbox "MaybeT" <> " constructor to wrap up the monadic sandwich. Note that the " <> fbox "liftM" <> " here works in the inner monad, just like the do-block wrapped by "
  <> fbox "MaybeT" <> " in the implementation of " <> bind <> " we saw early on was in the inner monad."




implementingT :: LaTeXC l => l
implementingT =
  subsubsection "The State transformer"
  <> "As an additional example, we will now have a detailed look at the implementation of " <> fbox "StateT" <> ". You might want to review the appendix on the State monad before continuing."
  <> par <> "Just as the State monad might have been built upon the definition "
  <> format "  newtype State s a = State { runState :: (s -> (a,s)) }   \n"
  <> ", the " <> fbox "StateT" <> " transformer is built upon the definition:"
  <> format "  newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }   \n"
  <> fbox "StateT s m" <> " will have the following " <> fbox "Monad" <> " instance, here shown alongside the one for the base state monad:"
  <> "\n \n"
  <> margenesEstrechos tablaStateVsStateT
  <> "\n \n"
  <> par <> "Our definition of " <> fbox "return" <> " makes use of the " <> fbox "return" <> " function of the inner monad." <> bind <> " uses a do-block to perform a computation in the inner monad."
  <> paragraph "Note" <> "Incidentally, we can now finally explain why, in the appendix about " <> fbox "State" <> ", there is a " <> fbox "state" <> " function instead of a " <> fbox "State" <> " constructor. In the "
  <> fbox "transformers" <> " and " <> fbox "mtl" <> " packages, " <> fbox "State s" <> " is implemented as a type synonym for " <> fbox "StateT s Identity" <> ", with " 
  <> fbox "Identity" <> " being the dummy monad introduced in an exercise of the previous section. The resulting monad is equivalent to the one defined using " <> fbox "newtype" <> " that we have used up to now."
  <> par <> "If the combined monads " <> fbox "StateT s m" <> " are to be used as state monads, we will certainly want the all-important " <> fbox "get" <> " and " <> fbox "put" <> " operations. Here, we will show definitions in the style of the "
  <> fbox "mtl" <> " package. In addition to the monad transformers themselves, " <> fbox "mtl" <> " provides type classes for the essential operations of common monads. For instance, the " 
  <> fbox "MonadState" <> " class, found in " <> fbox "Control.Monad.State" <> ", has " <> fbox "get" <> " and " <> fbox "put" <> " as methods:"
  <> format "  instance (Monad m) => MonadState s (StateT s m) where   \n    get   = StateT $ \\s -> return (s,s)   \n    put s = StateT $ \\_ -> return ((),s)    \n"
  <> paragraph "Note" <> "The first line should be read as: " <> qts ("For any type " <> fbox "s" <> " and any instance of " <> fbox "Monad m" <> "; " <> fbox "s" <> " and " <> fbox "StateT s m" <> " together form an instance of " <> fbox "MonadState")
  <> ". " <> fbox "s" <> " and " <> fbox "m" <>" correspond to the state and the inner monad, respectively. " <> fbox "s" <> " is an independent part of the instance specification so that the methods can refer to it - for instance, the type of " <> fbox "put" <> " is " <> fbox ( "s " <> math (raw "\\,"<>to) <> "StateT s ()" ) <> "."
  <> par <> "There are " <> fbox "MonadState" <> " instances for state monads wrapped by other transformers, such as "
  <> format "  MonadState s m => MonadState s (MaybeT m)   \n"
  <> "They bring us extra convenience by making it unnecessary to lift uses of " <> fbox "get" <> " and " <> fbox "put" <> " explicitly, as the " <> fbox "MonadState" <> " instance for the combined monads handles the lifting for us."
  <> par <> "It can also be useful to lift instances that might be available for the inner monad to the combined monad. For instance, all combined monads in which " <> fbox "StateT" <> " is used with an instance of " <> fbox "MonadPlus" <> " can be made instances of " <> fbox "MonadPlus" <> ":"
  <> format "  instance (MonadPlus m) => MonadPlus (StateT s m) where   \n    mzero = StateT $ \\_ -> mzero   \n    (StateT x1) `mplus` (StateT x2) = StateT $ \\s -> (x1 s) `mplus` (x2 s)   \n"
  <> "The implementations of " <> fbox "mzero" <> " and " <> fbox "mplus" <> " do the obvious thing; that is, delegating the actual work to the instance of the inner monad."
  <> par <> "Lest we forget, the monad transformer must have a " <> fbox "MonadTrans" <> " instance, so that we can use " <> fbox "lift" <> ":"
  <> format "  instance MonadTrans (StateT s) where   \n    lift c = StateT $ \\s -> c >>= (\\x -> return (x,s))    \n"
  <> "The " <> fbox "lift" <> " function creates a " <> fbox "StateT" <> " state transformation function that binds the computation in the inner monad to a function that packages the result with the input state. If, for instance, we apply " 
  <> fbox "StateT" <> " to the " <> fbox "List" <> " monad, a function that returns a list (i.e., a computation in the List monad) can be lifted into " <> fbox "StateT s [ ]" <> " where it becomes a function that returns a " <> fbox ( "StateT s" <> math (raw "\\,"<>to) <> " [(a,s)])" )
  <> ". I.e. the lifted computation produces " <> textit "multiple" <> " (value,state) pairs from its input state. This " <> qts "forks" <> " the computation in StateT, creating a different branch of the computation for each value in the list returned by the lifted function. Of course, applying "
  <> fbox "StateT" <> " to a different monad will produce different semantics for the " <> fbox "lift" <> " function."







