{-# LANGUAGE OverloadedStrings #-}

module TFG_monadasHaskell where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath


import TFG_maybeMonad (maybeMonad, bind)
import TFG_tablaMonadas
import TFG_additiveMonads
import TFG_monadTrans


-------- SIMBOLOS U OTRO CODIGO --------

comillas :: Char
comillas = toEnum 34

comillas'' :: String
comillas'' = [comillas]

comillas' :: LaTeXC l => l
comillas' = raw $ pack $ [comillas]

monad_definition :: LaTeXC l => l
monad_definition = verbatim (pack t) where
  t =
    "        class Monad m where \n"
    <> "        return :: a -> m a \n"
    <> "        (>>=)  :: m a -> (a -> m b) -> m b \n \n"
     
    <> "        (>>)   :: m a -> m b -> m b \n"
    <> "        fail   :: String -> m a \n"

monadLaws :: LaTeXC l => l
monadLaws = verbatim (pack t) where
  t =
    "  m >>= return      =   m                        -- right unit \n"
    <> "  return x >>= f    =   f x                      -- left unit \n\n"
    <> "  (m >>= f) >>= g   =   m >>= (\\x -> f x >>= g)  -- associativity \n"

monadComposition :: LaTeXC l => l
monadComposition = verbatim (pack t) where
  t =
   "  (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c \n"
   <> "  f >=> g   =   \\x -> f x >>= g"

f_y_a_dada_m :: LaTeXC l => l
f_y_a_dada_m = verbatim (pack t) where
  t =
    "  instance Functor Foo where \n"
    <> "  fmap = liftM \n\n"
    <> "  instance Applicative Foo where \n"
    <> "  pure = return \n"
    <> "  (<*>) = ap \n"

claseFunctor :: LaTeXC l => l
claseFunctor = verbatim (pack t) where
  t =
    "  class  Functor f  where \n" 
    <> "      fmap        :: (a -> b) -> f a -> f b"

reglasFunctor :: LaTeXC l => l
reglasFunctor = verbatim (pack t) where
  t =
    "  fmap id   =   id \n"
    <> "  fmap (g . f)   =   fmap g . fmap f"


ejMaybe1 :: LaTeXC l => l
ejMaybe1 = verbatim (pack t) where
  t = "    father :: Person -> Maybe Person \n" <> "    mother :: Person -> Maybe Person \n"

ejMaybe2 :: LaTeXC l => l
ejMaybe2 = verbatim (pack t) where
  t =
    "    maternalGrandfather :: Person -> Maybe Person \n"
    <> "    maternalGrandfather p = \n"
    <> "    case mother p of \n"
    <> "        Nothing -> Nothing \n"
    <> "        Just mom -> father mom \n"

ejMaybe3 :: LaTeXC l => l
ejMaybe3 = verbatim (pack t) where
  t =
    "bothGrandfathers :: Person -> Maybe (Person, Person) \n"
    <> "bothGrandfathers p = \n"
    <> "    case father p of \n"
    <> "        Nothing -> Nothing \n"
    <> "        Just dad -> \n"
    <> "            case father dad of \n"
    <> "                Nothing -> Nothing \n"
    <> "                Just gf1 ->                    -- 1st grandfather \n"
    <> "                    case mother p of \n"
    <> "                        Nothing -> Nothing \n"
    <> "                        Just mom -> \n"
    <> "                            case father mom of \n"
    <> "                                Nothing -> Nothing \n"
    <> "                                Just gf2 ->    -- 2nd grandfather \n"
    <> "                                    Just (gf1, gf2) \n"

ejMaybe4 :: LaTeXC l => l
ejMaybe4 = verbatim (pack t) where
  t =
    "bothGrandfathers p = \n"
    <> "    father p >>= \n"
    <> "        (\\dad -> father dad >>= \n"
    <> "            (\\gf1 -> mother p >>=   \n"
    <> "-- this line works as `\\_ -> mother p', but naming gf1 allows later return \n"
    <> "                (\\mom -> father mom >>= \n"
    <> "                     (\\gf2 -> return (gf1,gf2) )))) \n"

imperativeCode :: LaTeXC l => l
imperativeCode = verbatim (pack t) where
  t =
    "    bothGrandfathers p = do { \n"
    <> "        dad <- father p; \n"
    <> "        gf1 <- father dad; \n"
    <> "        mom <- mother p; \n"
    <> "        gf2 <- father mom; \n"
    <> "        return (gf1, gf2); \n"
    <> "    } \n"

monadAltDef :: LaTeXC l => l
monadAltDef = verbatim (pack t) where
  t =
    "    fmap   :: (a -> b) -> M a -> M b  -- functor \n"
    <> "    return :: a -> M a \n"
    <> "    join   :: M (M a) -> M a \n"

altDefEqs :: LaTeXC l => l
altDefEqs = verbatim (pack t) where
  t = "    fmap f x = x >>= (return . f) \n" <> "    join x   = x >>= id \n"


ejemplosFunctor :: LaTeXC l => l
ejemplosFunctor = ej1 <> ej2 <> ej3
  where
    ej1 = par <> "The Maybe functor" <> verbatim (pack maybeFunctor)
    maybeFunctor =  "instance Functor Maybe where  \n " <> "        fmap f Nothing  = Nothing \n " <> "        fmap f (Just x)   =  Just (f x) \n " 
    
    ej2 = par <> "The List functor" <> verbatim (pack listFunc)
    listFunc = "instance Functor [] where \n " <> "        fmap = map \n "
    
    ej3 = par <> "The Tree functor" <> verbatim (pack treeFunc)
    treeFunc = "instance Functor Tree where \n " <> "        fmap f (Leaf x) = Leaf (f x) \n " <> "        fmap f (Branch left right) = Branch (fmap f left) (fmap f right) \n " 


infix_fmap :: LaTeXC l => l
infix_fmap = scriptsize (fbox (math "(<$>)" ))

ejInfixFmap :: LaTeXC l => l
ejInfixFmap = verbatim (pack t) where
  t =
    "  Prelude> negate <$> Just 2 \n"
    <> "  Just (-2) \n"

appEj1 :: LaTeXC l => l
appEj1 = verbatim (pack t) where
  t =
    "  Prelude> :t (+) <$> Just 2 \n"
    <> "  (+) <$> Just 2 :: Num a => Maybe (a -> a) \n"

apply :: LaTeXC l => l
apply = scriptsize (fbox (math "(<*>)" ))

appEj2 :: LaTeXC l => l
appEj2 = verbatim (pack t) where
  t =
    "  Prelude> (+) <$> Just 2 <*> Just 3                      \n"
    <> "  Just 5                                            \n \n"
    <> "  Prelude> :t (<*>)                                    \n"
    <> "  (<*>) :: Applicative f => f (a -> b) -> f a -> f b   \n"

appClassCode :: LaTeXC l => l
appClassCode = verbatim (pack t) where
  t =
    "  class (Functor f) => Applicative f where \n"
    <> "      pure  :: a -> f a                 \n"
    <> "      (<*>) :: f (a -> b) -> f a -> f b \n"

appMaybe :: LaTeXC l => l
appMaybe = verbatim (pack t) where
  t =
    "  instance Applicative Maybe where            \n"
    <> "      pure                  = Just         \n"
    <> "      (Just f) <*> (Just x) = Just (f x)   \n"
    <> "      _        <*> _        = Nothing      \n"  

leyesApp :: LaTeXC l => l
leyesApp = verbatim (pack t) where
  t =
    "  pure id <*> v = v                            -- Identity \n"
    <> "  pure f <*> pure x = pure (f x)               -- Homomorphism \n"
    <> "  u <*> pure y = pure ($ y) <*> u              -- Interchange \n"
    <> "  pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition \n"

doNot1 :: LaTeXC l => l
doNot1 = verbatim (pack t) where
  t =
    "  putStr " <> comillas'' <> "Hello" <> comillas'' <> " >>        \n"
    <> "  putStr " <> comillas'' <> " " <> comillas'' <> " >>         \n"
    <> "  putStr " <> comillas'' <> "world!" <> comillas'' <> " >>    \n"
    <> "  putStr " <> comillas'' <> "\\n" <> comillas'' <> "          \n"

doNot2 :: LaTeXC l => l
doNot2 = verbatim (pack t) where
  t =
    "  do putStr " <> comillas'' <> "Hello" <> comillas'' <> "        \n"
    <> "     putStr " <> comillas'' <> " " <> comillas'' <> "         \n"
    <> "     putStr " <> comillas'' <> "world!" <> comillas'' <> "    \n"
    <> "     putStr " <> comillas'' <> "\\n" <> comillas'' <> "       \n"

doNot3 :: LaTeXC l => l
doNot3 = verbatim (pack t) where
  t =
    "  do action1    \n"
    <> "     action2 \n"
    <> "     action3 \n"

doNot4 :: LaTeXC l => l
doNot4 = verbatim (pack t) where
  t =
    "  action1 >>     \n"
    <> "  do action2  \n"
    <> "     action3  \n"

doNot5 :: LaTeXC l => l
doNot5 = verbatim (pack t) where
  t =
    "  do x1 <- action1     \n"
    <> "     x2 <- action2  \n"
    <> "     action3 x1 x2  \n"

doNot6 :: LaTeXC l => l
doNot6 = verbatim (pack t) where
  t = "  action1 >>= \\ x1 -> action2 >>= \\ x2 -> action3 x1 x2 \n"

doNot7 :: LaTeXC l => l
doNot7 = verbatim (pack t) where
  t =
    "  action1                            \n"
    <> "    >>=                           \n"
    <> "      \\ x1 -> action2            \n"
    <> "        >>=                       \n"
    <> "          \\ x2 -> action3 x1 x2  \n"

doNot8 :: LaTeXC l => l
doNot8 = verbatim (pack t) where
  t =
    "  action1 >>= \\ x1 ->       \n"
    <> "    action2 >>= \\ x2 ->  \n"
    <> "      action3 x1 x2       \n"

doNot9 :: LaTeXC l => l
doNot9 = verbatim (pack t) where
  t =
    "  action1 >>= \\ x1 ->     \n"
    <> "  action2 >>= \\ x2 ->  \n"
    <> "  action3 x1 x2         \n"

doNot10 :: LaTeXC l => l
doNot10 = verbatim (pack t) where
  t =
    "  do Just x1 <- action1      \n"
    <> "     x2      <- action2   \n"
    <> "     action3 x1 x2        \n"

doNot11 :: LaTeXC l => l
doNot11 = verbatim (pack t) where
  t =
    "  action1 >>= f                                   \n"
    <> "  where f (Just x1) = do x2 <- action2         \n"
    <> "                         action3 x1 x2         \n"
    <> "        f _         = fail "<>comillas''<>"..."<>comillas''<>" -- A compiler-generated message. \n"


doNot12 :: LaTeXC l => l
doNot12 = verbatim (pack t) where
  t =
    "  nameDo :: IO () \n"
    <> "  nameDo = do putStr "<>comillas''<>"What is your first name? "<>comillas''<>"                \n"
    <> "              first <- getLine                                  \n"
    <> "              putStr "<>comillas''<>"And your last name? "<>comillas''<>"                     \n"
    <> "              last <- getLine                                   \n"
    <> "              let full = first ++ "<>comillas''<>" "<>comillas''<>" ++ last                   \n"
    <> "              putStrLn ("<>comillas''<>"Pleased to meet you, "<>comillas''<>" ++ full ++ "<>comillas''<>"!"<>comillas''<>") \n"


doNot13 :: LaTeXC l => l
doNot13 = verbatim (pack t) where
  t =
    "  nameLambda :: IO ()                                                  \n"
    <> "  nameLambda = putStr "<>comillas''<>"What is your first name? "<>comillas''<>" >>                \n"
    <> "               getLine >>= \\ first ->                               \n"
    <> "               putStr "<>comillas''<>"And your last name? "<>comillas''<>">>                     \n"
    <> "               getLine >>= \\ last ->                                \n"
    <> "               let full = first ++ "<>comillas''<>" "<>comillas''<>" ++ last                      \n"
    <> "               in putStrLn ("<>comillas''<>"Pleased to meet you, "<>comillas''<>" ++ full ++ "<>comillas''<>"!"<>comillas''<>") \n"

doNot14 :: LaTeXC l => l
doNot14 = verbatim (pack t) where
  t =
    "  nameReturn :: IO String                                               \n"
    <> "  nameReturn = do putStr "<>comillas''<>"What is your first name? "<>comillas''<>"                 \n"
    <> "                  first <- getLine                                   \n"
    <> "                  putStr "<>comillas''<>"And your last name? "<>comillas''<>"                      \n"
    <> "                  last <- getLine                                    \n"
    <> "                  let full = first ++ "<>comillas''<>" "<>comillas''<>" ++ last                    \n"
    <> "                  putStrLn ("<>comillas''<>"Pleased to meet you, "<>comillas''<>" ++ full ++ "<>comillas''<>"!"<>comillas''<>")  \n"
    <> "                  return full                                        \n"

doNot15 :: LaTeXC l => l
doNot15 = verbatim (pack t) where
  t =
    "  greetAndSeeYou :: IO () \n"
    <> "  greetAndSeeYou = do name <- nameReturn \n"
    <> "                      putStrLn ("<>comillas''<>"See you, "<>comillas''<>" ++ name ++ "<>comillas''<>"!"<>comillas''<>") \n"

doNot16 :: LaTeXC l => l
doNot16 = verbatim (pack t) where
  t =
    "  nameReturnAndCarryOn = do \n"
    <> "    putStr "<>comillas''<>"What is your first name? "<>comillas''<>"               \n"
    <> "    first <- getLine                            \n"
    <> "    putStr "<>comillas''<>"And your last name? "<>comillas''<>"              \n"
    <> "    last <- getLine                             \n"
    <> "    let full = first++"<>comillas''<>" "<>comillas''<>"++last                  \n"
    <> "    putStrLn ("<>comillas''<>"Pleased to meet you, "<>comillas''<>"++full++"<>comillas''<>"!"<>comillas''<>") \n"
    <> "    return full                                  \n"
    <> "    putStrLn "<>comillas''<>"I am not finished yet!"<>comillas''<>"            \n"

doNot17 :: LaTeXC l => l
doNot17 = verbatim (pack t) where
  t =
    "  greetAndSeeYou :: IO () \n"
    <> "  greetAndSeeYou =     \n"                                                       
    <> "    nameReturn >>= \\ name -> putStrLn ("<>comillas''<>"See you, "<>comillas''<>" ++ name ++ "<>comillas''<>"!"<>comillas''<>")  \n"

doNot18 :: LaTeXC l => l
doNot18 = verbatim (pack t) where
  t =
    "  printSeeYou :: String -> IO ()                                \n"
    <> "  printSeeYou name = putStrLn ("<>comillas''<>"See you, "<>comillas''<>" ++ name ++ "<>comillas''<>"!"<>comillas''<>")   \n"

doNot19 :: LaTeXC l => l
doNot19 = verbatim (pack t) where
  t =
    "  greetAndSeeYou :: IO ()                         \n"
    <> "  greetAndSeeYou = nameReturn >>= printSeeYou  \n"

doNot20 :: LaTeXC l => l
doNot20 = verbatim (pack t) where
  t =
    "  seeYou :: String -> String                    \n"
    <> "  seeYou name = "<>comillas''<>"See you, "<>comillas''<>" ++ name ++ "<>comillas''<>"!"<>comillas''<>"   \n"

doNot21 :: LaTeXC l => l
doNot21 = verbatim (pack t) where
  t =
    "  -- Reminder: fmap f m == m >>= return . f == liftM f m   \n"
    <> "  greetAndSeeYou :: IO ()                               \n"
    <> "  greetAndSeeYou = fmap seeYou nameReturn >>= putStrLn  \n"

----------------------------------------




cap2 :: LaTeXC l => l
cap2 = 
   chapter "Haskell Monad class"
   <> introCap2
   <> section "Why? the IO monad" <> elPorque
   <> section "What?" <> elQue 
   <> subsection "Starring: Monad typeclass" <> docMonadasReducida
   <> newpage
   <> section "Pre-example with Maybe" <> preejemplo
   <> subsection "Notions of Computation" <> notionsComp
   <> newpage
   <> section "Who?" <> comandoKind
   <> newpage
   <> section "How?" <> lasReglas
   <> subsection "Alternative definitions" <> altDefs
   <> subsection "Note: avoiding the prerequisites" <> functorYapplicativeDadaMonad
   <> newpage
   <> section "Prerequisites: Functor and Applicative typeclasses" <> functor_y_applicative
   <> newpage
   <> subsection "Applicative functor laws" <> appClass
   <> newpage
   <> section (textit "do" <> " notation") <> doNotation
   <> newpage
   <> section "Additive monads (MonadPlus)" <> addMonads
   <> newpage
   <> section "Monad transformers" <> monadTrans
       



introCap2 :: LaTeXC l => l
introCap2 = "As seen in the previous chapter, monad definition in Mathematics lies beyond a long and winding path (we saw both ends, but the in-between theory was omitted); etimology doesn't help either, leading to: "
        <> par <> "Monad (n.): " <> comillas' <> "Unity, arithmetical unit" <> comillas' <> ", 1610s, from Late Latin " <> textit "monas" <> " (genitive " <> textit "monadis" <> "), from Greek " <> textit "monas "
        <> comillas' <> "unit" <> comillas' <> ", from " <> textit "monos" <> comillas' <> "alone" <> comillas' <> " (see " <> textit "mono" <> "). In Leibnitz's philosophy, " <> comillas' <> "an ultimate unit of being"
        <> comillas' <> " (1748). Related: " <> textit "Monadic" <> "."
        <> "\n\n So, as even more questions arise, lets sort them up: "




elPorque :: LaTeXC l => l
elPorque = 
   " Beyond internally calculating values, we want our programs to interact with the world. The most common beginners' program in any language simply displays a " <> qts "hello world" <> " greeting on the screen. Here's a Haskell version: "
   <> verbatim ( pack ("\n Prelude> putStrLn " ++ [comillas] ++ "Hello, World!" ++ [comillas]) )
   <> " So now you should be thinking, " <> qts "what is the type of the putStrLn function?" <> " It takes a " <> fbox "String" <> " and gives... um... what? What do we call that?"
   <> " The program doesn't get something back that it can use in another function. Instead, the result involves having the computer change the screen. "
   <> " In other words, it does something in the world outside of the program. What type could that have? Let's see what GHCi tells us: "
   <> verbatim (pack "\n Prelude> :t putStrLn \n putStrLn :: String -> IO ()" )
   <> qts "IO" <> " stands for " <> qts "input and output" <> ". Wherever there is " <> fbox "IO" <> " in a type, interaction with the world outside the program is involved"
   <> ". We'll call these " <> fbox "IO" <> " values " <> textit "actions" <> ". The other part of the " <> fbox "IO" <> " type, in this case " <> fbox "()" <> ", is the type of the return value of the action"
   <> "; that is, the type of what it gives back to the program (as opposed to what it does outside the program). " <> fbox "()" <> " (pronounced as " <> qts "unit" <> ") is a type that only contains one value also called "
   <> fbox "()" <> " (effectively a tuple with zero elements). Since " <> fbox "putStrLn" <> " sends output to the world but doesn't return anything to the program, " <> fbox "()" <> " is used as a placeholder"
   <> ". We might read " <> fbox "IO ()" <> " as " <> qts ( "action which returns " <> fbox "()") <> "."
   <> "What makes IO actually work? Lots of things happen behind the scenes to take us from " <> fbox "putStrLn" <> " to pixels in the screen, but we don't need to understand any of the details to write our programs"
   <> ". A complete Haskell program is actually a big IO action. In a compiled program, this action is called " 
   <> fbox "main" <> " and has type " <> fbox "IO ()" <> ". From this point of view, to write a Haskell program is to combine actions and functions to form the overall action " <> fbox "main"
   <> " that will be executed when the program is run. The compiler takes care of instructing the computer on how to do this. "




elQue :: LaTeXC l => l
elQue = 
  par <> "Monads are by no means limited to input and output. Monads support a whole range of things like exceptions, state, non-determinism, continuations, coroutines, and more"
  <> ". In fact, thanks to the versatility of monads, none of these constructs needed to be built into Haskell as a language; instead, they are defined by the standard libraries."




docMonadasReducida :: LaTeXC l => l
docMonadasReducida = 
  "In Haskell, the " <> fbox "Monad" <> " type class is used to implement monads. It is provided by the Control.Monad module and included in the Prelude. "
  <> " The class has the following methods:"
  <> footnote "For the full definition of Monad in the Prelude, look Appendix B"
  <> monad_definition
  <> "The core methods are " <> fbox "return" <> " and " <> bind <> " (which is pronounced " <> qts "bind" <> "). \n"
  <> "Aside from return and bind, notice the two additional functions " <> scriptsize (fbox (math  "(>>)")) <> " and " <> fbox "fail" <> ". \n "
  <> "The operator " <> scriptsize (fbox (math "(>>)")) <> " called " <> qts "then" <> " is a mere convenience and commonly implemented as \n"
  <> verbatim ( pack "        m >> n   =   m >>= \\_ -> n" )
  <> scriptsize (fbox (math "(>>)")) <> " sequences two monadic actions when the second action does not involve the result of the first, which is common for monads like " <> fbox "IO" <> ". \n"
  <> "The function " <> fbox "fail" <> " handles pattern match failures in " <> fbox "do" <> " notation. It's an unfortunate technical necessity and doesn't really have anything to do with monads. "
  <> "You are advised not to call " <> fbox "fail" <> " directly in your code."



preejemplo :: LaTeXC l => l
preejemplo =
  "For a concrete example, take the " <> fbox "Maybe" <> " monad. The type constructor is " <> fbox "m = Maybe" <> ", while " <> fbox "return" <> " and " <> bind <> " are defined like this: "
  <> maybeMonad
  <> fbox "Maybe" <> " is the monad, and " <> fbox "return" <> " brings a value into it by wrapping it with " <> fbox "Just" <> ". As for " <> bind <> ", it takes a " <> fbox "m :: Maybe a" <> " value and a " 
  <> fbox ( math ("g :: a" <> to) <> " Maybe b") <> " function. If " <> fbox "m" <> " is " <> fbox "Nothing" <> ", there is nothing to do and the result is " <> fbox "Nothing" <> ". Otherwise, in the " <> fbox "Just x" 
  <> " case, " <> fbox "g" <> " is applied to " <> fbox "x" <> ", the underlying value wrapped in " <> fbox "Just" <> ", to give a " <> fbox "Maybe b" <> " result, which might be " <> fbox "Nothing" 
  <> ", depending on what " <> fbox "g" <> " does to " <> fbox "x" <> ". To sum it all up, if there is an underlying value in " <> fbox "m" <> ", we apply " <> fbox "g" <> " to it, which brings the underlying value back into the " <> fbox "Maybe" <> " monad."
  <> par <> "The key first step to understand how " <> fbox "return" <> " and " <> bind <> " work is tracking which values and arguments are monadic and which ones aren't. As in so many other cases, type signatures are our guide to the process."
  <> subsubsection "Motivation: Maybe"
  <> "To see the usefulness of " <> bind <> " and the " <> fbox "Maybe" <> " monad, consider the following example: Imagine a family database that provides two functions" 
  <> ejMaybe1
  <> "These look up the name of someone's father or mother. In case our database is missing some information, " <> fbox "Maybe" <> " allows us to return a " <> fbox "Nothing" <> " value instead of crashing the program"
  <> ". Let's combine our functions to query various grandparents. For instance, the following function looks up the maternal grandfather:"
  <> ejMaybe2
  <> "Or consider a function that checks whether both grandfathers are in the database:"
  <> ejMaybe3
  <> "What a mouthful! Every single query might fail by returning " <> fbox "Nothing" <> " and the whole function must fail with " <> fbox "Nothing" <> " if that happens. "
  <> "Clearly there as to be a better way to write that instead of repeating the case of " <> fbox "Nothing" <> " again and again! Indeed, that's what the " <> fbox "Maybe" <> " monad is set out to do"
  <> ". For instance, the function retrieving the maternal grandfather has exactly the same structure as the " <> bind <> " operator, so we can rewrite it as:"
  <> verbatim "    maternalGrandfather p   =   mother p >>= father \n"
  <> "With the help of lambda expressions and return, we can rewrite the two grandfathers function as well:"
  <> ejMaybe4
  <> "While these nested lambda expressions may look confusing to you, the thing to take away here is that " <> bind <> " releases us from listing all the " <> fbox "Nothing" <> "s, shifting the focus back to the interesting part of the code."
  <> " To be a little more precise: The result of " <> fbox "father p" <> " is a monadic value (in this case, either " <> fbox "Just dad" <> " or " <> fbox "Nothing" <> ", depending on whether " <> fbox "p" <> "'s dad is in the database). As the "
  <> fbox "father" <> " function takes a regular (non-monadic value), the " <> bind <> " feeds " <> fbox "p" <> "'s dad to it as a " <> textit "non-monadic" <> " value. The result of " <> fbox "father dad" <> " is then monadic again, and the process continues."
  <> par <> "So, " <> bind <> " helps us pass non-monadic values to functions without leaving a monad. In the case of the " <> fbox "Maybe" <> " monad, the monadic aspect is the qualifier that we don't know with certainty whether the value will be found."



notionsComp :: LaTeXC l => l
notionsComp =
  "We've seen how " <> bind <> " and " <> fbox "return" <> " are very handy for removing boilerplate code that crops up when using " <> fbox "Maybe" <> ". That, however, is not enough to justify why monads matter so much"
  <> ". We will continue our monad studies by rewriting the two-grandfathers function using " <> fbox "do" <> " notation with explicit braces and semicolons. Depending on your experience with other programming languages, you may find this very suggestive:"
  <> imperativeCode
  <> "If this looks like a code snippet of an imperative programming language to you, that's because it is. In particular, this imperative language supports " <> textit "exceptions" <> ": father and mother are functions that might fail to produce results"
  <> ", i.e. raise an exception, and when that happens, the whole " <> fbox "do" <> "-block will fail, i.e. terminate with an exception."
  <> par <> "In other words, the expression " <> fbox "father p" <> ", which has type " <> fbox "Maybe Person" <> ", is interpreted as a statement of an imperative language that returns a " <> fbox "Person" <> " as result"
  <> textbf ( ". This is true for all monads: a value of type " <> fbox "M a" <> " is interpreted as a statement of an imperative language that returns a value of type " <> fbox "a" <> " as result; and the semantics of this language are determined by the monad " <> fbox "M" <> "." )
  <> footnote ( "By `semantics', we mean what the language allows you to say. In the case of " <> fbox "Maybe" <> ", the semantics allow us to express failure, as statements may fail to produce a result, leading to the statements that follow it being skipped." )
  <> " \n\n Under this interpretation, the bind operator " <> bind <> " is simply a function version of the semicolon. Just like a " <> fbox "let" <> " expression can be written as a function application,"
  <> verbatim " let x = foo in x + 3      corresponds to      (\\x -> x + 3) foo"
  <> "an assignment and semicolon can be written as the bind operator:"
  <> verbatim ( "      x <- foo; return (x + 3) \n" <> "      corresponds to  \n" <> "      foo >>= (\\x -> return (x + 3))" )
  <> "The " <> fbox "return" <> " function lifts a value " <> fbox "a" <> " to " <> fbox "M a" <> ", a full-fledged statement of the imperative language corresponding to the monad " <> fbox "M" <> "."
  <> par <> "Different semantics of the imperative language correspond to different monads. The following table shows the classic selection that every Haskell programmer should know. If the idea behind monads is still unclear to you, studying each of the "
  <> "examples in the following chapters will not only give you a well-rounded toolbox but also help you understand the common abstraction behind them. \n\n"
  <> center tablaMonadas
  <> par <> "Furthermore, these different semantics need not occur in isolation. As we will see in a few chapters, it is possible to mix and match them by using monad transformers to combine the semantics of multiple monads in a single monad."



comandoKind :: LaTeXC l => l
comandoKind =
  "The first observation when studying the monad definition in the Prelude is that its a type class, just like " <> fbox "Eq" <> ", " <> fbox "Ord" <> " or " <> fbox "Num"
  <> ". As such, instead of " <> textit "what is a monad?" <> " we should be asking ourselves " <> textit "what is TO BE monad?"
  <> " - because that's how classes work and help us, enhancing types with new capabilities; for example, "
  <> "the " <> fbox "Eq" <> " and " <> fbox "Ord" <> " classes provide comparability between that type elements, and the " <> fbox "Num" <> " class allows the use of " <> fbox (math "+") <>  " or " <> fbox (math "*") <> "."
  <> par <> "In fact, with a little help with the GHCi command " <> fbox ":kind" <> " we can already answer the question " <> textit "what types can be made instance of the Monad class?" <> ". Check it yourself!"
  <> probandoKind
  <> "Looking closely the kind of Monad, we get that " <> textbf "only 1-parameterized types" <> " are allowed to be instantiated in the " <> fbox "Monad" <> " class. This is, types like "
  <> fbox "Maybe a" <> ", " <> fbox "[a]" <> " or " <> fbox "(a)" <> "; but not " <> fbox "Int" <> ", " <> fbox "Bool" <> " or " <> fbox "Either a b" <> " (however, " <> fbox "Either Int a" <> " will do the trick). \n"
  <> "As soon as GHCi meets the " <> (qts $ fbox "instance Monad Int where") <> " line, the following error will be displayed: "
  <> verbatim (pack t) 
  <> par <> "You don't program with kinds: the compiler infers them for itself. But if you get parameterized types wrong then the compiler will report a kind error."
  where
    t=    
      "The first argument of `Monad' should have kind `* -> *', "
      <> "\n but `Int' has kind `*' "
      <> "\n In the instance declaration for `Monad Int' "



probandoKind :: LaTeXC l => l
probandoKind = verbatim (pack t) where
  t =
    "Prelude> :k Bool \n"
    <> "Bool :: * \n"
    <> "Prelude> :k Int \n"
    <> "Int :: * \n"
    <> "Prelude> :k [] \n"
    <> "[] :: * -> * \n"
    <> "Prelude> :k [Int] \n"
    <> "[Int] :: * \n"
    <> "Prelude> :k Maybe \n"
    <> "Maybe :: * -> * \n"
    <> "Prelude> :k (,,,,,) \n"
    <> "(,,,,,) :: * -> * -> * -> * -> * -> * -> * \n"
    <> "Prelude> :k Eq \n"
    <> "Eq :: * -> Constraint \n"
    <> "Prelude> :k Ord \n"
    <> "Ord :: * -> Constraint \n"
    <> "Prelude> :k Num \n"
    <> "Num :: * -> Constraint \n"
    <> "Prelude> :k Show \n"
    <> "Show :: * -> Constraint \n"
    <> "Prelude> :k Functor \n"
    <> "Functor :: (* -> *) -> Constraint \n"
    <> "Prelude> :k Monad \n"
    <> "Monad :: (* -> *) -> Constraint \n\n"
    <> "Prelude> :t Constraint \n\n"
    <> "<interactive>:1:1: Not in scope: data constructor `Constraint' \n"
    <> "Prelude> :k Constraint \n\n"
    <> "<interactive>:1:1: \n"
    <> "    Not in scope: type constructor or class `Constraint' \n"
    <> "Prelude> :m GHC.Prim  \n"
    <> "Prelude GHC.Prim> :k Constraint \n"
    <> "Constraint :: BOX \n"
    <> "Prelude GHC.Prim> :k BOX \n"
    <> "BOX :: BOX \n\n"
    <> "Prelude> :m Data.Monoid \n"
    <> "Prelude Data.Monoid> :k Monoid \n"
    <> "Monoid :: * -> Constraint \n"



lasReglas :: LaTeXC l => l
lasReglas =
   subsection "The Rules " <> "In Haskell, every instance of the " <> fbox "Monad" <> " type class (and thus all implementations of bind " <> bind <> " and " <> fbox "return" <> " ) must obey the following three laws:"
   <> monadLaws
   <> par <> "The behavior of " <> fbox "return" <> " is specified by the left and right unit laws. They state that " <> fbox "return" <> " doesn't perform any computation, it just collects values."
   <> par <> "The law of associativity makes sure that (like the semicolon) the bind operator " <> bind <> " only cares about the order of computations, not about their nesting."
   <> " The associativity of the " <> textit "then" <> " operator " <> scriptsize (fbox (math "(>>)")) <> " is a special case:"
   <> verbatim "  (m >> n) >> o  =  m >> (n >> o) \n"
   <> subsection "Monadic composition " <> "It is easier to picture the associativity of bind by recasting the law as"
   <> verbatim "  (f >=> g) >=> h  =  f >=> (g >=> h) \n"
   <> "where " <> scriptsize (fbox (math "(>=>)")) <> " is the " <> textbf "monad composition operator" <> ", a close analogue of the function composition operator " <> fbox "(.)" <> ", only with flipped arguments. It is defined as:"
   <> monadComposition
   <> "We can also flip monad composition to go the other direction using " <> scriptsize (fbox (math "(<=<)")) <> "." 



altDefs :: LaTeXC l => l
altDefs = 
  "Monads originally come from a branch of mathematics called Category Theory. Fortunately, it is entirely unnecessary to understand category theory in order to understand and use monads in Haskell"
  <> ". The definition of monads in Category Theory actually uses a slightly different presentation. Translated into Haskell, this presentation gives an alternative yet equivalent definition of a monad which can give us some additional insight."
  <> par <> "So far, we have defined monads in terms of " <> bind <> " and " <> fbox "return" <> ". The alternative definition, instead, starts with monads as functors with two additional combinators: "
  <> monadAltDef
  <> "(As will be discussed in the section on the functor class, a functor "
  <> fbox "M" <> " can be thought of as container, so that " <> fbox "M a" <> qts " contains" <> " values of type " <> fbox "a" 
  <> ", with a corresponding mapping function, i.e. " <> fbox "fmap" <> ", that allows functions to be applied to values inside it.)"
  <> "\n Under this interpretation, the functions behave as follows:"
  <> itemize (fmapDesc <> returnDesc <> joinDesc)
  <> "With these functions, the bind combinator can be defined as follows:"
  <> verbatim ( pack "    m >>= g = join (fmap g m)" )
  <> "Likewise, we could give a definition of " <> fbox "fmap" <> " and " <> fbox "join" <> " in terms of " <> bind <> " and " <> fbox "return" <> ":"
  <> altDefEqs
  <> "At this point we might, with good reason, conclude that all monads are by definition functors as well. That is indeed the case, both according to category theory and when programming in Haskell"
  <> ". A final observation is that " <> fbox "Control.Monad" <> " defines " <> fbox "liftM" <> ", a function with a strangely familiar type signature... "
  <> verbatim ( pack "    liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r \n" )
  <> "As you might suspect, " <> fbox "liftM" <> " is merely " <> fbox "fmap" <> " implemented with " <> bind <> " and " <> fbox "return"
  <> ", just as we have done above. For a properly implemented monad with a matching " <> fbox "Functor" <> " (that is, any " <> textit "sensible" <> " monad) " 
  <> fbox "liftM" <> " and " <> fbox "fmap" <> " are interchangeable."
    where
      fmapDesc = item Nothing <> fbox "fmap" <> " applies a given function to every element in a container"
      returnDesc = item Nothing <> fbox "return" <> " packages an element into a container"
      joinDesc = item Nothing <> fbox "join" <> " takes a container of containers and flattens it into a single container"





functorYapplicativeDadaMonad :: LaTeXC l => l
functorYapplicativeDadaMonad =
   "While following the next few chapters, you will likely want to write instances of " <> fbox "Monad" <> " and try them out, be it to run the examples in this text or to do other experiments you might think of. However, "
   <> fbox "Applicative" <> " being a superclass of " <> fbox "Monad" <> " means that implementing " <> fbox "Monad" <> " requires providing " <> fbox "Functor" <> " and " <> fbox "Applicative" 
   <> " instances as well. At this point of the report, that would be somewhat of an annoyance, especially given that we have not discussed " <> fbox "Applicative" <> " yet! As a workaround, once you have written the "
   <> fbox "Monad" <> " instance you can use the functions in " <> fbox "Control.Monad" <> " to fill in the " <> fbox "Functor" <> " and " <> fbox "Applicative" <> " implementations, as follows:"
   <> f_y_a_dada_m
   <> "We will find out what " <> fbox "pure" <> ", " <> scriptsize (fbox (math "(<*>)")) <> " and " <> fbox "ap" <> " are in due course."



functor_y_applicative :: LaTeXC l => l
functor_y_applicative = 
   "implementing " <> fbox "Monad" <> " requires providing " <> fbox "Functor" <> " and " <> fbox "Applicative" <> " instances as well."
   <> subsubsection "Functor class"
   <> fbox "Functor" <> " is a Prelude class for types which can be mapped over. It has a single method, called " <> fbox "fmap" 
   <> ". The class is defined as follows:" 
   <> claseFunctor 
   <> "Some examples: " <> verbatim "\n"
   <> ejemplosFunctor
   <> paragraph "The functor laws" 
   <> "When providing a new instance of " <> fbox "Functor" <> ", you should ensure it satisfies the two functor laws. There is nothing mysterious about these laws;" 
   <> " their role is to guarantee fmap behaves sanely and actually performs a mapping operation (as opposed to some other nonsense)."
   <> footnote "Some examples of nonsense that the laws rule out: removing or adding elements from a list, reversing a list, changing a Just-value into a Nothing"
   <> " The laws are:"
   <> reglasFunctor
   <> newpage
   <> subsubsection "Applicative functors"
   <> "Like monads, applicative functors are functors with extra laws and operations; in fact, " <> fbox "Applicative" <> " is an intermediate class between " <> fbox "Functor" <> " and " <> fbox "Monad"
   <> ". It enables the " <> textit "applicative style" <> ", a convenient way of structuring functorial computations, and also provides means to express a number of important patterns."
   <> par <> "Note: For extra convenience, " <> fbox "fmap" <> " has an infix synonym, " <> infix_fmap <> ". It often helps readability, and also suggests how " <> fbox "fmap" <> " can be seen as a different kind of function application."
   <> ejInfixFmap
   <> par <> "As useful as it is, " <> fbox "fmap" <> " isn't much help if we want to apply a function of two arguments to functorial values. For instance, how could we sum "
   <> fbox "Just 2" <> " and " <> fbox "Just 3" <> "? The brute force approach would be extracting the values from the " <> fbox "Maybe" <> " wrapper. That, however, would mean having to do tedious checks for "
   <> fbox "Nothing" <> ". Even worse: in a different " <> fbox "Functor" <> " extracting the value might not even be an option (just think about IO)."
   <> par <> "We could use " <> fbox "fmap" <> " to partially apply " <> (fbox $ math "(+)") <> " to the first argument:"
   <> appEj1
   <> "But now we are stuck: we have a function and a value both wrapped in " <> fbox "Maybe" <> ", and no way of applying one to the other. What we would like to have is an operator with a type akin to "
   <> verbatim ( pack "  f (a -> b) -> f a -> f b" )
   <> "  to apply functions in the context of a functor. That operator is called " <> apply <> ", check this:"
   <> appEj2
   <> par <> apply <> " is one of the methods of " <> fbox "Applicative" <> " the type class of " <> textit "applicative functors" <> " - functors that support function application within their contexts"
   <> ". \n Expressions such as " 
   <> verbatim (pack " (+) <$> Just 2 <*> Just 3 ") <> " are said to be written in " <> textit "applicative style" <> ", which is as close as we can get to regular function application while working with a functor. \n"
   <> "If you pretend for a moment the " <> infix_fmap <> ", " <> apply <> " and " <> fbox "Just" <> " aren't there, our example looks just like " <> fbox "(+) 2 3" <> "."



appClass :: LaTeXC l => l
appClass =
  "The definition of " <> fbox "Applicative" <> " is:"
  <> appClassCode
  <> "Beyond " <> apply <> ", the class has a second method, " <> fbox "pure" <> ", which brings arbitrary values into the functor. As an example, let's have a look at the " <> fbox "Maybe" <> " instance:"
  <> appMaybe
  <> "It doesn't do anything surprising: " <> fbox "pure" <> " wraps the value with " <> fbox "Just" <> "; " <> apply <> " applies the function to the value if both exists, and results in " <> fbox "Nothing" <> " otherwise."
  <> paragraph "Note" <> "For the lack of a better shorthand, in what follows we will use the word " <> textit "morphism" <> " to refer to the values to the left of " <> apply <> ", which fit the type " <> verb "Applicative f => f (a -> b)" <> "; that is, the function-like things inserted into an applicative functor. "
  <> par <> "Just like " <> fbox "Functor" <> ", " <> fbox "Applicative" <> " has a set of laws which reasonable instances should follow. They are:" 
  <> leyesApp
  <> "Those laws are a bit of a mouthful. They become easier to understand if you think of " <> fbox "pure" <> " as a way to inject values into the functor in a default, featureless way, so that the result is as close as possible to the plain value. Thus:"
  <> itemize (law1<>law2<>law3<>law4)
  <> par <> "There is also a bonus law about the relation between " <> fbox "fmap" <> " and " <> apply <> ":"
  <> verbatim (pack "  fmap f x = pure f <*> x                      -- fmap \n" )
  <> "Applying a " <> qts "pure" <> " function with " <> apply <> " is equivalent to using " <> fbox "fmap" <> ". " <> textbf "This law is a consequence of the other ones, so you need not bother with proving it when writing instances of " <> fbox (textbf "Applicative") <> "."
    where
      law1 = item Nothing <> "The identity law says that applying the " <> fbox "pure id" <> " morphism does nothing, exactly like with the plain " <> fbox "id" <> " function."
      law2 = item Nothing <> "The homomorphism law says that applying a " <> qts "pure" <> " function to a " <> qts "pure" <> " value is the same than applying the function to the value in the normal way and then using " <> fbox "pure" <> " on the result. In a sense, that means " <> fbox "pure" <> " preserves function application."
      law3 = item Nothing <> "The interchange law says that applying a morphism to a " <> qts "pure" <> " value " <> fbox "pure y" <> " is the same as applying pure " <> fbox "($ y)"
        <> " to the morphism. No surprises there - " <> fbox "($ y)" <> " is the function that supplies " <> fbox "y" <> " as argument to another function."
      law4 = item Nothing <> "The composition law says that if " <> apply <> " is used to compose morphisms the composition is associative, like plain function composition."
        <> footnote ( " With plain functions, we have " <> fbox " h . g . f = (h . g) . f = h . (g . f) " <> " That is why we never bother to use parentheses in the middle of " <> fbox "(.)" <> " chains." )



doNotation :: LaTeXC l => l
doNotation = 
  "Using " <> fbox "do" <> " blocks as an alternative monad syntax was introduced with an " <> fbox "IO" <> " example. Since the following examples all involve " <> fbox "IO" <> ", we will refer to the computations/monadic values as "
  <> textit "actions" <> " (as we did in the earlier parts of the report). Of course, " <> fbox "do" <> " works with any monad; there is nothing specific about " <> fbox "IO" <> " in how it works."
  <> subsection ("Translating the " <> textit "then" <> " operator")
  <> "The " <> fbox (scriptsize (math "(>>)")) <> textit " (then)" <> " operator works almost identically in " <> fbox "do" <> " notation and in unsugared code. For example, suppose we have a chain of actions like the following one:"
  <> doNot1
  <> "We can rewrite that in " <> fbox "do" <> " notation as follows:"
  <> doNot2
  <> "This sequence of instructions nearly matches that in any imperative language. In Haskell, we can chain any actions as long as all of them are in the same monad. In the context of the " <> fbox "IO" <> " monad, the actions include writing to a file, opening a network connection, or asking the user for input."
  <> par <> "Here's the step-by-step translation of " <> fbox "do" <> " notation to unsugared Haskell code: "
  <> doNot3
  <> "becomes"
  <> doNot4
  <> "and so on, until the " <> fbox "do" <> " block is empty."
  <> subsection ("Translating the " <> textit "bind" <> " operator")
  <> "The " <> bind <> " is a bit more difficult to translate from and to " <> fbox "do" <> " notation. " <> bind <> " passes a value, namely the result of an action or function, downstream in the binding sequence. "
  <> fbox "do" <> " notation assigns a variable name to the passed value using the " <> verb "<-" <> "."
  <> doNot5
  <> par <> fbox "x1" <> " and " <> fbox "x2" <> " are the results of " <> fbox "action1" <> " and " <> fbox "action2" <> ". If, for instance, "
  <> fbox "action1" <> " is an " <> fbox "IO Integer" <> " then " <> fbox "x1" <> " will be bound to an " <> fbox "Integer" <> ". The stored values are passed as arguments to " <> fbox "action3" 
  <> ", which returns a third action. The " <> fbox "do" <> " block is broadly equivalent to the following vanilla Haskell snippet:"
  <> doNot6
  <> "The second argument of " <> bind <> " is a function specifying what to do with the result of the action passed as first argument. Thus, chains of lambdas pass the results downstream. Remember that, without extra parentheses, a lambda extends all the way to the end of the expression."
  <> fbox "x1" <> " is still in scope at the point we call " <> fbox "action3" <> ". We can rewrite the chain of lambdas more legibly by using separate lines and indentation:"
  <> doNot7
  <> "That shows the scope of each lambda function clearly. To group things more like the " <> fbox "do" <> " notation, we could show it like this:"
  <> doNot8
  <> "These presentation differences are only a matter of assisting readability"
  <> ". Actually, the indentation isn't needed in this case. This is equally valid: " 
  <> doNot9
  <> subsection ( "The " <> textit "fail" <> " method")
  <> "Above, we said the snippet with lambdas was " <> qts "broadly equivalent" <> " to the " <> fbox "do" <> " block. The translation is not exact because the " <> fbox "do" <> " notation adds special handling of pattern match failures. When placed at the left of either "
  <> fbox (scriptsize (math "<-")) <> " or " <> fbox (scriptsize (math "->")) <> ", " <> fbox "x1" <> " and " <> fbox "x2" <> " are patterns being matched. Therefore, if " <> fbox "action1" <> " returned a " <> fbox "Maybe Integer" <> " we could write a " <> fbox "do" <> " block like this... "
  <> doNot10
  <> " ...and " <> fbox "x1" <> " be an " <> fbox "Integer" <> ". In such a case, what happens if " <> fbox "action1" <> " returns " <> fbox "Nothing" <> "? Ordinarily, the program would crash with a non-exhaustive patterns error, just like the one we get when calling " 
  <> fbox "head" <> " on an empty list. With " <> fbox "do" <> " notation, however, failures are handled with the " <> fbox "fail" <> " method for the relevant monad. The " <> fbox "do" <> " block above translates to:"
  <> doNot11
  <> "What " <> fbox "fail" <> " actually does depends on the monad instance. Though it will often rethrow the pattern matching error, monads that incorporate some sort of error handling may deal with the failure in their own specific ways. For instance, "
  <> fbox "Maybe" <> " has " <> fbox "fail _ = Nothing" <> "; analogously, for the list monad " <> fbox "fail _ = []" <> "."
  <> footnote "This explains why pattern matching failures in list comprehensions are silently ignored."
  <> par <> "The fail method is an artifact of " <> fbox "do" <> " notation. Rather than calling " <> fbox "fail" <> " directly, you should rely on automatic handling of pattern match failures whenever you are sure that " <> fbox "fail" <> " will do something sensible for the monad you are using."
  <> subsection "Example: user-interactive program"
  <> paragraph "Note for non-ghci users" 
  <> "We are going to interact with the user, so we will use " <> fbox "putStr" <> " and " <> fbox "getLine" <> " alternately. To avoid unexpected results in the output, we must disable output buffering when importing "
  <> fbox "System.IO" <> ".\n \n To do this, put " <> fbox "hSetBuffering stdout NoBuffering" <> " at the top of your code. To handle this otherwise, you would explicitly flush the output buffer before each interaction with the user (namely a "
  <> fbox "getLine" <> ") using " <> fbox "hFlush stdout" <> ". If you are testing this code with ghci, you don't have such problems."
  <> par <> "Consider this simple program that asks the user for their first and last names:"
  <> doNot12
  <> "A possible translation into vanilla monadic code:"
  <> doNot13
  <> "In cases like this, where we just want to chain several actions, the imperative style of " <> fbox "do" <> " notation feels natural and convenient. In comparison, monadic code with explicit binds and lambdas is something of an acquired taste."
  <> par <> " Notice that the first example above includes a " <> fbox "let" <> " statement in the " <> fbox "do" <> " block. The de-sugared version is simply a regular " <> fbox "let" <> " expression where the " <> fbox "in" <> " part is whatever follows from the " <> fbox "do" <> " syntax."
  <> subsection "Returning values"
  <> "The last statement in " <> fbox "do" <> " notation is the overall result of the " <> fbox "do" <> " block. In the previous example, the result was of the type " <> fbox "IO ()" <> ", i.e. an empty value in the " <> fbox "IO" <> " monad."
  <> par <> "Suppose that we want to rewrite the example but return an " <> fbox "IO String" <> " with the acquired name. All we need to do is add a " <> fbox "return" <> ":"
  <> doNot14
  <> "This example will " <> qts "return" <> " the full name as a string inside the " <> fbox "IO" <> " monad, which can then be utilized downstream elsewhere:"
  <> doNot15
  <> "Here, " <> fbox "nameReturn" <> " will be run and the returned result (called " <> qts "full" <> " in the " <> fbox "nameReturn" <> " function) will be assigned to the variable " <> qts "name" <> " in our new function. The greeting part of "
  <> fbox "nameReturn" <> " will be printed to the screen because that is part of the calculation process. Then, the additional " <> qts "see you" <> " message will print as well, and the final returned value is back to being " <> fbox "IO ()" <> "."
  <> par <> "If you know imperative languages like C, you might think " <> fbox "return" <> " in Haskell matches " <> fbox "return" <> " elsewhere. A small variation on the example will dispel that impression:"
  <> doNot16
  <> "The string in the extra line " <> textit "will" <> " be printed out because " <> fbox "return" <> " is not a final statement interrupting the flow (as it would be in C and other languages). Indeed, the type of "
  <> fbox "nameReturnAndCarryOn" <> " is " <> fbox "IO ()" <> ", - the type of the final " <> fbox "putStrLn" <> " action. After the function is called, the " <> fbox "IO String" <> " created by the "
  <> fbox "return full" <> " will disappear without a trace."
  <> subsection "Just sugar"
  <> "As a syntactical convenience, " <> fbox "do" <> " notation does not add anything essential, but it is often preferable for clarity and style. However, " <> fbox "do" <> " is never used for a single action. The Haskell " <> qts "Hello world" <> " is simply:"
  <> verbatim (pack ("main = putStrLn " <> comillas'' <> "Hello world!" <> comillas'' <> "\n") )
  <> "Snippets like this one are totally redundant:"
  <> verbatim ( pack " fooRedundant = do x <- bar \n                    return x \n" )
  <> "Thanks to the monad laws, we can and should write simply:"
  <> verbatim (pack "  foo = bar \n")
  <> "A subtle but crucial point relates to function composition: As we already know, the " <> fbox "greetAndSeeYou" <> " action in the section just above could be rewritten as:"
  <> doNot17
  <> "While you might find the lambda a little unsightly, suppose we had a " <> fbox "printSeeYou" <> " function defined elsewhere:"
  <> doNot18
  <> "Now, we can have a clean function definition with neither lambdas or " <> fbox "do" <> ":"
  <> doNot19
  <> "Or, if we have a " <> textit "non-monadic " <> fbox "seeYou" <> " function:"
  <> doNot20
  <> "Then we can write:"
  <> doNot21
  <> "Keep this last example with " <> fbox "fmap" <> " in mind; we will soon return to using non-monadic functions in monadic code, and " <> fbox "fmap" <> " will be useful there."
























































