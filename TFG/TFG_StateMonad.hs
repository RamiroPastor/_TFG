{-# LANGUAGE OverloadedStrings #-}

module TFG_StateMonad where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath

import TFG_maybeMonad (bind)





-------- SIMBOLOS U OTRO CODIGO --------

c :: Char
c = toEnum 34

format :: LaTeXC c => String -> c
format = verbatim . pack


ejRandom :: LaTeXC l => l
ejRandom = format texto where
  texto =
    "  GHCi> :m System.Random              \n"
    <> "  GHCi> :t randomIO                \n"
    <> "  randomIO :: Random a => IO a     \n"
    <> "  GHCi> randomIO                   \n"
    <> "  -1557093684                      \n"
    <> "  GHCi> randomIO                   \n"
    <> "  1342278538                       \n"


ejDice :: LaTeXC l => l
ejDice = format t where
  t =
    "  import Control.Monad                                                   \n"
    <> "  import System.Random                                             \n \n"
    <> "  rollDiceIO :: IO (Int, Int)                                         \n"
    <> "  rollDiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))         \n"

ejRandom2 :: LaTeXC l => l
ejRandom2 = format t where
  t =
    "  GHCi> :m System.Random                               \n"
    <> "  GHCi> let generator = mkStdGen 0                  \n"
    <> "    -- ``0'' is our seed                              \n"
    <> "  GHCi> :t generator                                \n"
    <> "  generator :: StdGen                               \n"
    <> "  GHCi> generator                                   \n"
    <> "  1 1                                               \n"
    <> "  GHCi> :t random                                   \n"
    <> "  random :: (RandomGen g, Random a) => g -> (a, g)  \n"
    <> "  GHCi> random generator :: (Int, StdGen)           \n"
    <> "  (2092838931,1601120196 1655838864)                \n"

ejRandom3 :: LaTeXC l => l
ejRandom3 = format t where
  t =
    "  GHCi> let randInt = fst . random $ generator :: Int   \n"
    <> "  GHCi> randInt                                      \n"
    <> "  2092838931                                         \n"

ejRandom4 :: LaTeXC l => l
ejRandom4 = format t where
  t =
    "  GHCi> let (randInt, generator') = random generator :: (Int, StdGen)     \n"
    <> "  GHCi> randInt                                                        \n"
    <> "    -- Same value                                                      \n"
    <> "  2092838931                                                           \n"
    <> "  GHCi> random generator' :: (Int, StdGen)                             \n"
    <> "    -- Using new generator' returned from ``random generator''         \n"
    <> "  (-2143208520,439883729 1872071452)                                   \n"

clumsyRollDice :: LaTeXC l => l
clumsyRollDice = format t where
  t =
    "  clumsyRollDice :: (Int, Int)                     \n"
    <> "  clumsyRollDice = (n, m)                       \n"
    <> "          where                                 \n"
    <> "          (n, g) = randomR (1,6) (mkStdGen 0)   \n"
    <> "          (m, _) = randomR (1,6) g              \n"

stateBind :: LaTeXC l => l
stateBind = format t where
  t =
    "  (>>=) :: State s a -> (a -> State s b) -> State s b    \n"
    <> "  pr >>= k = state $ \\ st ->                          \n"
    <> "     let (x, st') = runState pr st                    \n"
    <> "         -- Running the first processor on st.        \n"
    <> "     in runState (k x) st'                            \n"
    <> "         -- Running the second processor on st'.      \n"

masFuncs :: LaTeXC l => l
masFuncs = format t where
  t =
    "  evalState :: State s a -> s -> a              \n"
    <> "  evalState pr st = fst (runState pr st) \n  \n"
    <> "  execState :: State s a -> s -> s           \n"
    <> "  execState pr st = snd (runState pr st)     \n"

imports :: LaTeXC l => l
imports = format t where
  t =
    "  import Control.Monad.Trans.State   \n"
    <> "  import System.Random            \n"

rollDie :: LaTeXC l => l
rollDie = format t where
  t =
    "  rollDie :: State StdGen Int                                          \n"
    <> "  rollDie = do generator <- get                                     \n"
    <> "               let (value, newGenerator) = randomR (1,6) generator  \n"
    <> "               put newGenerator                                     \n"
    <> "               return value                                         \n"

ejRandom5 :: LaTeXC l => l
ejRandom5 = format t where
  t =
    "  GHCi> evalState getRandom (mkStdGen 0) :: Bool        \n"
    <> "  True                                               \n"
    <> "  GHCi> evalState getRandom (mkStdGen 0) :: Double   \n"
    <> "  0.9872770354820595                                 \n"
    <> "  GHCi> evalState getRandom (mkStdGen 0) :: Integer  \n"
    <> "  2092838931                                         \n"

ejRandom6 :: LaTeXC l => l
ejRandom6 = format t where
  t =
    "  someTypes :: State StdGen (Int, Float, Char)                                  \n"
    <> "  someTypes = liftM3 (,,) getRandom getRandom getRandom                  \n  \n"
    <> "  allTypes :: State StdGen (Int, Float, Char, Integer, Double, Bool, Int)    \n"
    <> "  allTypes = liftM (,,,,,,) getRandom                                        \n"
    <> "                       `ap` getRandom                                        \n"
    <> "                       `ap` getRandom                                        \n"
    <> "                       `ap` getRandom                                        \n"
    <> "                       `ap` getRandom                                        \n"
    <> "                       `ap` getRandom                                        \n"
    <> "                       `ap` getRandom                                        \n"

tipoCurioso :: LaTeXC l => l
tipoCurioso = format t where
  t =
    "  GHCi>:t liftM (,,,,,,) getRandom                                        \n"
    <> "  liftM (,,,,,) getRandom :: (Random a1) =>                            \n"
    <> "                             State StdGen (b -> c -> d -> e -> f -> g  \n"
    <> "                                 -> (a1, b, c, d, e, f, g))            \n"





----------------------------------------



anexo8 :: LaTeXC l => l
anexo8 =
  chapter "Appendix: The State monad (Random Number Generation)"
  <> "If you have programmed in any other language before, you likely wrote some functions that " <> qts "kept state" <> ". For those new to the concept, a " <> textit "state"
  <> " is one or more variables that are required to perform some computation but are not among the arguments of the relevant function. Object-oriented languages, like C++, suggest extensive use of state variables within objects in the form of member variables"
  <> ". Programs written in procedural languages, like C, typically use variables declared outside the current scope to keep track of state."
  <> par <> "In Haskell, however, such techniques are not as straightforward to apply. They require mutable variables and imply functions will have hidden dependencies, which is at odds with Haskell's functional purity"
  <> ". Fortunately, in most cases it is possible to avoid such extra complications and keep track of state in a functionally pure way"
  <> ". We do so by passing the state information from one function to the next, thus making the hidden dependencies explicit. The "
  <> fbox "State" <> " type is a tool crafted to make this process of threading state through functions more convenient. In this chapter, we will see how it can assist us in a typical problem involving state: generating pseudo-random numbers."
  <> section "Pseudo-Random Numbers" <> prn
  <> section ("Introducing " <> textit "State") <> introState
  <> elFinal




prn :: LaTeXC l => l
prn =
  "Generating actual random numbers is far from easy. Computer programs almost always use " <> textit "pseudo" <> "-random numbers instead. They are called " <> qts "pseudo"
  <> " because they are not truly random. Rather, they are genererated by algorithms (the pseudo-random number generators) which take an initial state (commonly called the seed) and produce from it a sequence of numbers that have the appearance of being random."
  <> footnote "A common source of seeds is the current date and time as given by the internal clock of the computer. Assuming the clock is functioning correctly, it can provide unique seeds suitable for most day-to-day needs (as opposed to applications which demand high-quality randomness, as in cryptography or statistics)"
  <> par <> "Every time a pseudo-random number is requested, state somewhere must be updated, so that the generator can be ready for producing a fresh, different random number"
  <> ". Sequences of pseudo-random numbers can be replicated exactly if the initial seed and the generating algorithm are known."
  <> subsection "Implementation in Haskell"
  <> "Producing a pseudo-random number in most programming languages is very simple: there is a function somewhere in the libraries that provides a pseudo-random value (perhaps even a truly random one, depending on how it is implemented). Haskell has a similar one in the " <> fbox "System.Random" <> " module from the " <> fbox "random" <> " package:"
  <> ejRandom
  <> par <> fbox "randomIO" <> " is an " <> fbox "IO" <> " action. It couldn't be otherwise, as it makes use of mutable state, which is kept out of reach from our Haskell programs. Thanks to this hidden dependency, the pseudo-random values it gives back can be different every time."
  <> subsection "Example: rolling dice"
  <> "Suppose we are coding a game in which at some point we need an element of chance. In real-life games that is often obtained by means of dice. So, let's create a dice-throwing function. We'll use the " <> fbox "IO" <> " function " <> fbox "randomIO" <> ", which allows us to specify a range from which the pseudo-random values will be taken. For a 6 die, the call will be " <> fbox "randomIO (1,6)" <> "."
  <> ejDice
  <> par <> "That function rolls two dice. Here, " <> fbox "liftM2" <> " is used to make the non-monadic two-argument function " <> fbox "(,)" <> " work within a monad. The " <> fbox "(,)" <> " is the non-infix version of the tuple constructor. Thus, the two die rolls will be returned as a tuple in " <> fbox "IO" <> "."
  <> subsubsection ("Getting rid of " <> textit "IO")
  <> "A disadvantage of " <> fbox "randomIO" <> " is that it requires us to use " <> fbox "IO" <> " and store our state outside the program, where we can't control what happens to it. We would rather only use I/O when there is an unavoidable reason to interact with the outside world."
  <> par <> "To avoid bringing " <> fbox "IO" <> " into play, we can build a " <> textit "local" <> " generator. The " <> fbox "random" <> " and " <> fbox "mkStdGen" <> " functions in " <> fbox "System.Random" <> " allow us to generate tuples containing a pseudo-random number together with an updated generator to use the next time the function is called."
  <> ejRandom2
  <> paragraph "Note " <> "In " <> fbox "random generator :: (Int, StdGen)" <> ", we use the " <> fbox "::" <> " to introduce a " <> textit "type annotation"
  <> ", which is essentially a type signature that we can put in the middle of an expression. Here, we are saying that the expression to the right, " <> fbox "random generator"
  <> " has type " <> fbox "(Int, StdGen)" <> ". It makes sense to use a type annotation here because, as we will discuss later, " <> fbox "random" <> " can produce values of different types, so if we want it to give us an " <> fbox "Int" <> " we'd better specify it in some way."
  <> par <> "While we managed to avoid " <> fbox "IO" <> ", there are new problems. First and foremost, if we want to use " <> fbox "generator" <> " to get random numbers, the obvious definition..."
  <> ejRandom3
  <> "... is useless. It will always give back the same value, " <> fbox "2092838931" <> ", as the same generator in the same state will be used every time. To solve that, we can take the second member of the tuple (that is, the new generator) and feed it to a " <> textit "new" <> " call to " <> fbox "random" <> ":"
  <> ejRandom4
  <> "That, of course, is clumsy and rather tedious, as we now need to deal with the fuss of carefully passing the generator around."
  <> subsection "Dice without IO"
  <> "We can re-do our dice throw with our new approach using the " <> fbox "randomR" <> " function:"
  <> format "  GHCi> randomR (1,6) (mkStdGen 0) \n  (6, 40014 40692) \n"
  <> "The resulting tuple combines the result of throwing a single die with a new generator. A simple implementation for throwing two dice is then:"
  <> clumsyRollDice
  <> par <> "The implementation of " <> fbox "clumsyRollDice" <> " works as an one-off, but we have to manually pass the generator " <> fbox "g" <> " from one " <> fbox "where"
  <> " clause to the other. This approach becomes increasingly cumbersome as our programs get more complex, which means we have more values to shift around. It is also error-prone: what if we pass one of the middle generators to the wrong line in the " <> fbox "where" <> " clause?"
  <> par <> "What we really need is a way to automate the extraction of the second member of the tuple (i.e. the new generator) and feed it to a new call to " <> fbox "random"
  <> ". This is where the " <> fbox "State" <> " comes into the picture."





introState :: LaTeXC l => l
introState =
  paragraph "Note " <> "In this chapter we will use the state monad provided by the module " <> fbox "Control.Monad.Trans.State" <> " of the " <> fbox "transformers"
  <> " package. By reading Haskell code in the wild, you will soon meet " <> fbox "Control.Monad.State" <> ", a module of the closely related "
  <> fbox "mtl" <> " package. The differences between these two modules need not concern us at the moment; everything we discuss here also applies to the " <> fbox "mtl" <> " variant."
  <> par <> "The Haskell type " <> fbox "State" <> " describes functions that consume a state and produce both a result and an updated state, which are given back in a tuple."
  <> par <> "The state function is wrapped by a data type definition which comes along with a " <> fbox "runState" <> " accessor so that pattern matching becomes unnecessary. For our current purposes, the " <> fbox "State" <> " type might be defined as:"
  <> format "  newtype  State s a  =  State { runState :: s -> (a, s) } \n"
  <> "Here, " <> fbox "s" <> " is the type of the state, and " <> fbox "a" <> " the type of the produced result. Calling the type " <> fbox "State" <> " is arguably a bit of a misnomer because the wrapped value is not the state itself but a " <> textit "state processor" <> "."
  <> subsubsection "newtype"
  <> "Note that we defined the data type with the " <> fbox "newtype" <> " keyword, rather than the usual " <> fbox "data" <> ". " <> fbox "newtype" <> " can be used only for types with just one constructor and just one field. "
  <> par <> "It ensures that the trivial wrapping and unwrapping of the single field is eliminated by the compiler. For that reason, simple wrapper types such as " <> fbox "State" <> " are usually defined with "
  <> fbox "newtype" <> ". Would defining a synonym with " <> fbox "type" <> " be enough in such cases? Not really, because " <> fbox "type" <> " does not allow us to define instances for the new data type, which is what we are about to do..."
  <> subsection ("Where did the " <> textit "State" <> " constructor go?")
  <> "When you start using " <> fbox "Control.Monad.Trans.State" <> ", you will quickly notice there is no " <> fbox "State"
  <> " constructor available. The " <> fbox "transformers" <> " package implements the " <> fbox "State" <> " type in a somewhat different way. The differences do not affect how we use or understand "
  <> fbox "State" <> "; except that, instead of a " <> fbox "State" <> " constructor, " <> fbox "Control.Monad.Trans.State" <> " exports a " <> fbox "state" <> " function,"
  <> format "  state :: (s -> (a, s)) -> State s a  \n"
  <> "which does the same job. As for " <> textit "why" <> " the implementation is not the obvious one we presented above, we will get back to that a few chapters down the road."
  <> subsection "Instantiating the monad"
  <> "So far, all we have done was to wrap a function type and give it a name. There is another ingredient, however: " <> fbox "State" <> " is a monad, and that gives us very handy ways of using it. Unlike the instances of " <> fbox "Functor" <> " or " <> fbox "Monad"
  <> " we have seen so far, " <> fbox "State" <> " has " <> textit "two" <> " type parameters. Since the type class only allows one parametrised parameter, the last one, we have to indicate the other one, " <> fbox "s" <> ", will be fixed."
  <> format "  instance Monad (State s) where \n"
  <> "That means there are actually " <> textit "many" <> " different " <> fbox "State" <> " monads, one for each possible type of state - "
  <> fbox "State String" <> ", " <> fbox "State Int" <> ", " <> fbox "State SomeLargeDataStructure" <> ", and so forth. Naturally, we only need to write one implementation of "
  <> fbox "return" <> " and " <> bind <> "; the methods will be able to deal with all choices of " <> fbox "s" <> "."
  <> par <> "The " <> fbox "return" <> " function is implemented as:"
  <> format "  return :: a -> State s a  \n  return x = state ( \\ st -> (x, st) )     \n "
  <> par <> "Giving a value (" <> fbox "x" <> ") to " <> fbox "return" <> " produces a function which takes a state (" <> fbox "st" <> ") and returns it unchanged, together with value we want to be returned. As a finishing step, the function is wrapped up with the " <> fbox "state" <> " function."
  <> par <> "Binding is a bit intricate:"
  <> stateBind
  <> par <> bind <> " is given a state processor (" <> fbox "pr" <> ") and a function (" <> fbox "k" <> ") that is used to create another processor from thGe result of the first one. The two processors are combined into a function that takes the "
  <> fbox "initial" <> " state (" <> fbox "st" <> ") and returns the " <> textit "second" <> " result and the " <> textit "third" <> " state (i.e. the output of the second processor). Overall, "
  <> bind <> " here allows us to run two state processors in sequence, while allowing the result of the first stage to influence what happens in the second one."
  <> par <> "One detail in the implementation is how " <> fbox "runState" <> " is used to undo the " <> fbox "State" <> " wrapping, so that we can reach the function that will be applied to the states. The type of " <> fbox "runState pr" <> ", for instance, is " <> verb "s -> (a, s)" <> "."
  <> subsection "Setting and accessing the State"
  <> "The monad instance allows us to manipulate various state processors, but you may at this point wonder where exactly the " <> textit "original" <> " state comes from in the first place. That issue is handily dealt with by the function " <> fbox "put" <> ": "
  <> format "  put newState = state $ \\_ -> ((), newState)  \n"
  <> "Given a state (the one we want to introduce), " <> fbox "put" <> " generates a state processor which ignores whatever state it receives, and gives back the state we originally provided to " <> fbox "put"
  <> ". Since we don't care about the result of this processor (all we want to do is to replace the state), the first element of the tuple will be " <> fbox "()" <> ", the universal placeholder value."
  <> par <> "As a counterpart to " <> fbox "put" <> ", there is " <> fbox "get" <> ":"
  <> format "  get = state $ \\st -> (st, st)  \n"
  <> "The resulting state processor gives back the state " <> fbox "st" <> " it is given in both as a result and as a state. That means the state will remain unchanged, and that a copy of it will be made available for us to manipulate."
  <> subsection "Getting Values and State"
  <> "As we have seen in the implementation of " <> bind <> ", " <> fbox "runState" <> " is used to unwrap the " <> fbox "State a b"
  <> " value to get the actual state processing function, which is then applied to some initial state. Other functions which are used in similar ways are " <> fbox "evalState" <> " and " <> fbox "execState"
  <> ". Given a " <> fbox "State a b" <> " and an initial state, the function " <> fbox "evalState" <> " will give back only the result value of the state processing, whereas " <> fbox "execState" <> " will give back just the new state."
  <> masFuncs
  <> subsection "Dice and state"
  <> "Time to use the " <> fbox "State" <> " monad for our dice throw examples."
  <> imports
  <> "We want to generate " <> fbox "Int" <> " dice throw results from a pseudo-random generator of type " <> fbox "StdGen" <> ". Therefore, the type of our state processors will be "
  <> fbox "State StdGen Int" <> ", which is equivalent to " <> fbox ( math ("StdGen \\;"<>to<>" (Int, StdGen)")) <> " bar the wrapping."
  <> par <> "We can now implement a processor that, given a " <> fbox "StdGen" <> " generator, produces a number between 1 and 6. Now, the type of " <> fbox "randomR" <> " is:"
  <> format "  -- The StdGen type we are using is an instance of RandomGen.  \n  randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)  \n"
  <> "Doesn't it look familiar? If we assume " <> fbox "a" <> " is " <> fbox "Int" <> " and " <> fbox "g" <> " is " <> fbox "StdGen" <> " it becomes:"
  <> format "  randomR (1, 6) :: StdGen -> (Int, StdGen)   \n"
  <> "We already have a state processing function! All that is missing is to wrap it with " <> fbox "state" <> ":"
  <> format "  rollDie :: State StdGen Int   \n  rollDie = state $ randomR (1, 6)   \n"
  <> "For illustrative purposes, we can use " <> fbox "get" <> ", " <> fbox "put" <> " and do-notation to write " <> fbox "rollDie" <> " in a very verbose way which displays explicitly each step of the state processing:"
  <> rollDie
  <> "Let's go through each of the steps:" <> enumerate (aux1 <> aux2 <> aux3 <> aux4) where
    aux1 = item Nothing <> "First, we take out the pseudo-random generator from the monadic context with " <> fbox (math "<-") <> ", so that we can manipulate it."
    aux2 = item Nothing <> "Then, we use the " <> fbox "randomR" <> " function to produce an integer between 1 and 6 using the generator we took. We also store the new generator graciously returned by " <> fbox "randomR" <> "."
    aux3 = item Nothing <> "We then set the state to be the " <> fbox "newGenerator" <> " using " <> fbox "put" <> ", so that any further " <> fbox "randomR" <> " in the do-block, or further on in a " <> bind <> " chain, will use a different pseudo-random generator."
    aux4 = item Nothing <> "Finally, we inject the result back into the " <> fbox "State StdGen" <> " monad using " <> fbox "return" <> "."





elFinal :: LaTeXC l => l
elFinal =
  "We can finally use our monadic die. As before, the initial generator state itself is produced by the " <> fbox "mkStdGen" <> " function."
  <> format "  GHCi> evalState rollDie (mkStdGen 0)  \n  6   \n"
  <> "Why have we involved monads and built such an intricate framework only to do exactly what " <> fbox "fst $ randomR (1,6)" <> " already does? Well, consider the following function:"
  <> format "  rollDice :: State StdGen (Int, Int)   \n  rollDice = liftM2 (,) rollDie rollDie   \n"
  <> "We obtain a function producing " <> textit "two" <> " pseudo-random numbers in a tuple. Note that these are in general different:"
  <> format "  GHCi> evalState rollDice (mkStdGen 666)  \n   (6,1)   \n"
  <> "Under the hood, state is being passed through " <> bind <> " from one " <> fbox "rollDie" <> " computation to the other. Doing that was previously very clunky using " <> fbox "randomR (1,6)"
  <> " alone because we had to pass state manually. Now, the monad instance is taking care of that for us. Assuming we know how to use the lifting functions, constructing intricate combinations of pseudo-random numbers (tuples, lists, whatever) has suddenly become much easier."
  <> section "Pseudo-random values of different types"
  <> "Until now, we have used only " <> fbox "Int" <> " as type of the value produced by the pseudo-random generator. However, looking at the type of " <> fbox "randomR" <> " shows we are not restricted to " <> fbox "Int"
  <> ". It can generate values of any type in the " <> fbox "Random" <> " class from " <> fbox "System.Random" <> ". There already are instances for " <> fbox "Int" <> ", " <> fbox "Char" <> ", " <> fbox "Integer" <> ", " <> fbox "Bool" <> ", "
  <> fbox "Double" <> " and " <> fbox "Float" <> ", so you can immediately generate any of those."
  <> par <> "Because " <> fbox "State StdGen" <> " is " <> qts "agnostic" <> " in regard to the type of the pseudo-random value it produces, we can write a similarly "
  <> qts "agnostic" <> " function that provides a pseudo-random value of unspecified type (as long as it is an instance of " <> fbox "Random" <> "):"
  <> format "  getRandom :: Random a => State StdGen a   \n  getRandom = state random  \n"
  <> "Compared to " <> fbox "rollDie" <> ", this function does not specify the " <> fbox "Int" <> " type in its signature and uses" <> fbox "random" <> " instead of "
  <> fbox "randomR" <> "; otherwise, it is just the same. " <> fbox "getRandom" <> " can be used for any instance of " <> fbox "Random" <> ":"
  <> ejRandom5
  <> "Indeed, it becomes quite easy to conjure all these at once:"
  <> ejRandom6
  <> "For " <> fbox "allTypes" <> ", since there is no " <> fbox "liftM7" <> " (the standard libraries only go to " <> fbox "liftM5" <> ") we have used the " <> fbox "ap" <> " function from "
  <> fbox "Control.Monad" <> " instead. " <> fbox "ap" <> " fits multiple computations into an application of a multiple argument function, which here is the (lifted) 7-element-tuple constructor. To understand " <> fbox "ap" <> "further, look at its signature:"
  <> format "  ap :: (Monad m) => m (a -> b) -> m a -> m b  \n"
  <> "Remember then that the type variable " <> fbox "a" <> " in Haskell can be replaced by a function type as well as a regular value one, and compare to:"
  <> tipoCurioso
  <> "The monad " <> fbox "m" <> " obviously becomes " <> fbox "State StdGen" <> ", while " <> fbox "ap" <> "'s first argument is a function"
  <> format "  b -> c -> d -> e -> f -> g -> (a1, b, c, d, e, f, g)"
  <> "Applying " <> fbox "ap" <> " over and over (in this case 6 times), we finally get to the point where " <> fbox "b" <> " is an actual value (in our case, a 7-element tuple), not another function. To sum it up, "
  <> fbox "ap" <> " applies a function-in-a-monad to a monadic value (compare with " <> fbox "liftM" <> " / " <> fbox "fmap" <> ", which applies a function " <> textit "not" <> " in a monad to a monadic value)."
  <> par <> "So much for understanding the implementation. Function " <> fbox "allTypes" <> " provides pseudo-random values for all default instances of " <> fbox "Random" <> "; an additional " <> fbox "Int" <> " is inserted at the end to prove that the generator is not the same, as the two " <> fbox "Int" <> "s will be different."
  <> format "  GHCi> evalState allTypes (mkStdGen 0)  \n  GHCi>(2092838931,9.953678e-4,'a',-868192881,0.4188001483955421,False,316817438)  \n"
