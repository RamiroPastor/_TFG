{-# LANGUAGE OverloadedStrings #-}



module TFG_System_Random where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Hyperref


import TFG_maybeMonad (maybeMonad, bind)
import TFG_monadasHaskell (appMaybe, apply)



-------- SIMBOLOS U OTRO CODIGO --------

comillas :: Char
comillas = toEnum 34

format :: LaTeXC l => String -> l
format = verbatim . pack

funcType :: LaTeXC l => l
funcType = fbox ( math ( "((" <> to <> ")"<>(raw "\\;")<>"r)" ) )

formatSymbol :: LaTeXC l => l -> l
formatSymbol = scriptsize . fbox . math



randomGenCode :: LaTeXC l => l
randomGenCode = format t where
  t =
    "  class RandomGen g where                                    \n  \n"
    <> "  Minimal complete definition                               \n"
    <> "    next, split                                           \n  \n"
    <> "  Methods                                                   \n"
    <> "    next     :: g -> (Int , g)                              \n"
    <> "    split    :: g -> (g , g)                                \n"
    <> "    genRange :: g -> (Int , Int)                            \n"           
    <> "    genRange _ = (minBound , maxBound)                    \n  \n" 
    <> "  Instances                                                 \n"
    <> "    RandomGen StdGen                                      \n  \n"


stdGenCode :: LaTeXC l => l
stdGenCode = format t where
  t =
    "  data StdGen                                                \n  \n"
    <> "  Instances                                                 \n"
    <> "    Read StdGen                                             \n"
    <> "    Show StdGen                                             \n"
    <> "    RandmGen StdGen                                       \n  \n"
    <> "  mkStdGen :: Int -> StdGen                               \n  \n"


randomClassCode :: LaTeXC l => l
randomClassCode = format t where
  t =
    "  class Random a where                                   \n  \n"
    <> "  Minimal complete definition                           \n"
    <> "    randomR, random                                   \n  \n"
    <> "  Methods                                               \n"
    <> "    randomR   :: RandomGen g => (a, a) -> g -> (a, g)   \n"
    <> "    random    :: RandomGen g => g -> (a, g)             \n"
    <> "    randomRs  :: RandomGen g => (a, a) -> g -> [a]      \n"
    <> "    randoms   :: RandomGen g => g -> [a]                \n"
    <> "    randomRIO :: (a, a) -> IO a                         \n"
    <> "    randomIO  :: IO a                                 \n  \n"
    <> "  Instances                                             \n"
    <> "    Random Bool                                         \n"
    <> "    Random Char                                         \n"
    <> "    Random Double                                       \n"
    <> "    Random Float                                        \n"
    <> "    Random Int                                          \n"
    <> "    ...                                               \n  \n"


getTime :: LaTeXC l => l
getTime = format t where
  t =
    "  -- The standard nhc98 implementation of Time.ClockTime does not match         \n"
    <> "  -- the extended one expected in this module, so we lash-up a quick       \n"
    <> "  -- replacement here.                                                     \n"
    <> "  #ifdef __NHC__                                                           \n"
    <> "  foreign import ccall "<>[comillas]<>"time.h time"<>[comillas]<>" readtime :: Ptr CTime -> IO CTime     \n"
    <> "  getTime :: IO (Integer, Integer)                                         \n"
    <> "  getTime = do CTime t <- readtime nullPtr;  return (toInteger t, 0)       \n"
    <> "  #else                                                                    \n"
    <> "  getTime :: IO (Integer, Integer)                                         \n"
    <> "  getTime = do                                                             \n"
    <> "    utc <- getCurrentTime                                                  \n"
    <> "    let daytime = toRational $ utctDayTime utc                             \n"
    <> "    return $ quotRem (numerator daytime) (denominator daytime)             \n"
    <> "  #endif                                                                   \n"


makeStdRNG :: LaTeXC l => l
makeStdRNG = format t where
  t = 
    "  mkStdRNG :: Integer -> IO StdGen                                \n"
    <> "  mkStdRNG o = do                                            \n"
    <> "      ct          <- getCPUTime                              \n"
    <> "      (sec, psec) <- getTime                                 \n"
    <> "      return (createStdGen (sec * 12345 + psec + ct + o))    \n"


globalRNG :: LaTeXC l => l
globalRNG = format t where
  t =
    "  theStdGen :: IORef StdGen              \n"
    <> "  theStdGen  = unsafePerformIO $ do   \n"
    <> "     rng <- mkStdRNG 0                \n"
    <> "     newIORef rng                     \n"

----------------------------------------





anexo9 :: LaTeXC l => l
anexo9 = 
  chapter "The System.Random library"
  <> introSystemRandom
  <> section ("The " <> textit "RandomGen" <> " class")
  <> randomGenClass
  <> section ("The type " <> textit "StdGen" <> " and the global number generator")
  <> stdGenerators
  <> section ("Random vaues of other types: the " <> textit "Random" <> " class")
  <> randomClass
  <> section ("Other functions (that are " <> textbf "not" <> " exported)")
  <> sysRandHiddenFace



introSystemRandom :: LaTeXC l => l
introSystemRandom =
  "This library deals with the common task of pseudo-random number generation."
  <> footnote "This implementation uses the Portable Combined Generator of L'Ecuyerfor 32-bit computers, transliterated by Lennart Augustsson. It has a period of roughly 2.30584e18."
  <> " \n\n The library makes it possible to generate repeatable results, by starting with a specified initial random number generator, or to get different results on each run by using the system-initialised generator or by supplying a seed from some other source."
  <> footnote "For example, the third decimal of the internal clock"
  <> par <> "The library is split into two layers:"
  <> itemize (layer1 <> layer2)
    where
      layer1 = item Nothing <> "A core " <> textit "random number generator" <> " provides a supply of bits. The class " <> fbox "RandomGen" <> " provides a common interface to such generators. The library provides one instance of " <> fbox "RandomGen" <> ", the abstract data type " <> fbox "StdGen" <> ". Programmers may, of course, supply their own instances of " <> fbox "RandomGen" <> "."
      layer2 = item Nothing <> "The class " <> fbox "Random" <> " provides a way to extract values of a particular type from a random number generator. For example, the " <> fbox "Float" <> " instance of " <> fbox "Random" <> " allows one to generate random values of type " <> fbox "Float" <> "."




randomGenClass :: LaTeXC l => l
randomGenClass =
  "The class " <> fbox "RandomGen" <> " provides a common interface to random number generators. The most common approach is using the " <> fbox "StdGen" <> " type, presented in the next subsection."
  <> randomGenCode
  <> par <> "The " <> fbox "next" <> " operation returns an " <> fbox "Int" <> " that is uniformly distributed in the range returned by " <> fbox "genRange" <> " (including both end points), and a new generator."
  <> par <> "The " <> fbox "genRange" <> " operation yields the range of values returned by the generator."
  <> " The default definition spans the full range of " <> fbox "Int" <> "."
  <> par <> "It is required that:"
  <> itemize (aux1 <> aux2)
  <> "The second condition ensures that " <> fbox "genRange" <> " cannot examine its argument, and hence the value it returns can be determined only by the instance of " <> fbox "RandomGen" <> ". That in turn allows an implementation to make a single call to " <> fbox "genRange" 
  <> " to establish a generator's range, without being concerned that the generator returned by (say) " <> fbox "next" <> " might have a different range to the generator passed to " <> fbox "next" <> "."
  <> par <> "The " <> fbox "split" <> " operation allows one to obtain two distinct random number generators. This is very useful in functional programs (for example, when passing a random number generator down to recursive calls), but very little work has been done on statistically robust implementations of " <> fbox "split" <> "."
    where
      aux1 = item Nothing <> "If " <> verb "(a,b) = genRange g" <> ", then " <> verb "a < b" <> "."
      aux2 = item Nothing <> fbox "genRange" <> " always returns a pair of defined " <> fbox "Int" <> "s."
  



stdGenerators :: LaTeXC l => l
stdGenerators =
  subsection (textit "StdGen")
  <> stdGenCode
  <> par <> "The " <> fbox "StdGen" <> " instance of " <> fbox "RandomGen" <> " has a " <> fbox "genRange" <> " of at least 30 bits."
  <> par <> "The result of repeatedly using " <> fbox "next" <> " should be statistically robust."
  <> par <> "The " <> fbox "Show" <> " and " <> fbox "Read" <> " instances of " <> fbox "StdGen" <> " provide a primitive way to save the state of a random number generator. It is required that " <> verb "read (show g) == g" <> "."
  <> par <> "In addition, " <> fbox "reads" <> " may be used to map an arbitrary string (not necessarily one produced by " <> fbox "show" <> ") onto a value of type " <> fbox "StdGen" <> ". In general, the " <> fbox "Read" <> " instance of " <> fbox "StdGen" <> " has the following properties:"
  <> let 
      aux1 = item Nothing <> "It guarantees to succeed on any string. "
      aux2 = item Nothing <> "It guarantees to consume only a finite portion of the string. "
      aux3 = item Nothing <> "Different argument strings are likely to result in different results. "
     in
      itemize (aux1 <> aux2 <> aux3) 
  <> par <> "The function " <> fbox "mkStdGen" <> " provides an alternative way of producing an initial generator, by mapping an " <> fbox "Int" <> " into a generator. Again, distinct arguments should be likely to produce distinct generators."
  <> subsection "The global number generator"
  <> "There is a single, implicit, global random number generator of type " <> fbox "StdGen" <> ", held in some global variable maintained by the " <> fbox "IO" <> " monad. It is initialised automatically in some system-dependent fashion, for example, by using the time of day, or Linux's kernel random number generator. To get deterministic behaviour, use " <> fbox "setStdGen" <> "."
  <> format "  getStdRandom :: (StdGen -> (a, StdGen)) -> IO a   \n"
  <> par <> "Uses the supplied function to get a value from the current global random generator, and updates the global generator with the new generator returned by the function."
  <> format "  getStdGen :: IO StdGen    \n"
  <> par <> "Gets the global random number generator."
  <> format "  setStdGen :: StdGen -> IO ()  \n"
  <> par <> "Sets the global random number generator."
  <> format "  newStdGen :: IO StdGen       \n"
  <> par <> "Applies " <> fbox "split" <> " to the current global random generator, updates it with one of the results, and returns the other."




randomClass :: LaTeXC l => l
randomClass =
  "With a source of random number supply in hand, the " <> fbox "Random" <> " class allows the programmer to extract random values of a variety of types."
  <> randomClassCode
  <> par <> fbox "randomR" <> " takes a range " <> textit "(lo,hi)" <> " and a random number generator " <> fbox "g" <> ", and returns a random value uniformly distributed in the closed interval " <> textit "[lo,hi]" <> ", together with a new generator. "
  <> "It is unspecified what happens if " <> math "lo>hi" <> ". For continuous types there is no requirement that the values " <> textit "lo" <> " and " <> textit "hi" <> " are ever produced, but they may be, depending on the implementation and the interval."
  <> par <> fbox "random" <> " is the same as " <> fbox "randomR" <> ", but using a default range determined by the type:"
  <> itemize (aux1 <> aux2 <> aux3)
  <> par <> fbox "randomRs" <> " is a plural variant of " <> fbox "randomR" <> ", producing an infinite list of random values instead of returning a new generator."
  <> par <> fbox "randoms" <> " is a plural variant of " <> fbox "random" <> ", producing an infinite list of random values instead of returning a new generator."
  <> par <> fbox "randomRIO" <> " is a variant of " <> fbox "randomR" <> " that uses the global random number generator."
  <> par <> fbox "randomIO" <> " is a variant of " <> fbox "random" <> " that uses the global random number generator."
    where
      aux1 = item Nothing <> "For bounded types (instances of " <> fbox "Bounded" <> ", such as " <> fbox "Char" <> "), the range is normally the whole type."
      aux2 = item Nothing <> "For fractional types, the range is normally the semi-closed interval " <> textit "[0,1)" <> "."
      aux3 = item Nothing <> "For " <> fbox "Integer" <> ", the range is (arbitrarily) the range of " <> fbox "Int" <> "."




sysRandHiddenFace :: LaTeXC l => l
sysRandHiddenFace =
  "The following code is found in " 
  <> href [] (createURL "http://hackage.haskell.org/package/random-1.1/docs/src/System-Random.html") "System.Random" 
  <> footnote (url $ createURL "http://hackage.haskell.org/package/random-1.1/docs/src/System-Random.html") 
  <> " but not exported."
  <> subsection "The global number generator coding"
  <> "First, some note found early in the module"
  <> getTime
  <> "The function " <> fbox "getTime" <> " is used in:"
  <> makeStdRNG
  <> "Which finally gives us"
  <> globalRNG














