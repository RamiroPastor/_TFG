{-# LANGUAGE OverloadedStrings #-}

module TFG_IOMonad where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath

import TFG_maybeMonad (bind)





-------- SIMBOLOS U OTRO CODIGO --------

c :: Char
c = toEnum 34

c' :: LaTeXC l => l
c' = raw $ pack $ [c]


fullProgram :: LaTeXC l => l
fullProgram = verbatim (pack t) where
  t =
    "  module Main where \n \n"
    <> "  import Data.Char (toUpper) \n"
    <> "  import Control.Monad \n \n"
    <> "  main = putStrLn "<>[c]<>"Write your string: "<>[c]<>" >> fmap shout getLine >>= putStrLn \n \n"
    <> "  shout = map toUpper \n \n"

types :: LaTeXC l => l
types = verbatim (pack t) where
  t =
    "  main :: IO ()                                       \n"
    <> "  putStrLn :: String -> IO ()                      \n"
    <> "  "<>[c]<>"Write your string: "<>[c]<>" :: [Char]  \n"
    <> "  (>>) :: Monad m => m a -> m b -> m b             \n"
    <> "  fmap :: Functor m => (a -> b) -> m a -> m b      \n"
    <> "  shout :: [Char] -> [Char]                        \n"
    <> "  getLine :: IO String                             \n"
    <> "  (>>=) :: Monad m => m a -> (a -> m b) -> m b     \n"

fullProgDO :: LaTeXC l => l
fullProgDO = verbatim (pack t) where
  t =
    "  do putStrLn "<>[c]<>"Write your string: "<>[c]<>"  \n"
    <> "     string <- getLine                            \n"
    <> "     putStrLn (shout string)                      \n"

speakTo :: LaTeXC l => l
speakTo = verbatim (pack t) where
  t =
    "speakTo :: (String -> String) -> IO String                                                     \n"
    <> "  speakTo fSentence = fmap fSentence getLine                                             \n \n"
    <> "  -- Usage example.                                                                         \n"
    <> "  sayHello :: IO String                                                                     \n"
    <> "  sayHello = speakTo (\\name -> "<>[c]<>"Hello, "<>[c]<>" ++ name ++ "<>[c]<>"!"<>[c]<>")   \n"     
 
speakToType :: LaTeXC l => l
speakToType = verbatim (pack t) where
  t = "  speakTo :: (String -> String) -> String \n "
 
snippet1 :: LaTeXC l => l
snippet1 = verbatim (pack t) where
  t =
    "  int x;                  \n "
    <> "  scanf("<>[c]<>"%d"<>[c]<>", &x);     \n "
    <> "  printf("<>[c]<>"%d\\n"<>[c]<>", x);   \n "

snippet2 :: LaTeXC l => l
snippet2 = verbatim (pack t) where
  t =
    "  x <- readLn  \n "
    <> "  print x   \n "

snippet3 :: LaTeXC l => l
snippet3 = verbatim (pack t) where
  t = "  readLn >>= \\x -> print x  \n"

----------------------------------------



anexo6 :: LaTeXC l => l
anexo6 =
  chapter "Appendix: The IO (Input/Output) monad"
  <> "Haskell is a " <> textit "functional" <> " and " <> textit "lazy" <> " language. However, the real world effects of input/output operations can't be expressed through pure functions."
  <> " Furthermore, in most cases I/O can't be done lazily. Since lazy computations are only performed when their values become necessary, unfettered lazy I/O would make the order of execution of the real world effects unpredictable"
  <> ". Haskell addresses these issues through the " <> fbox "IO" <> " monad."
  <> section "Input/output and purity"
  <> "Haskell functions are " <> textit "pure" <> ": when given the same arguments, they return the same results. Pure functions are reliable and predictable; they ease debugging and validation"
  <> ". Test cases can also be set up easily since we can be sure that nothing other than the arguments will influence a function's result. Being entirely contained within the program, the Haskell compiler can evaluate functions thoroughly in order to optimize the compiled code."
  <> par <> "So, how do we manage actions like opening a network connection, writing a file, reading input from the outside world, or anything else that does something more than returning a calculated result? Well, the key is: "
  <> textit "these actions are not functions" <> ". The " <> fbox "IO" <> " monad is a means to represent actions as Haskell values, so that we can manipulate them with pure functions."
  <> section "Combining functions and I/O actions" <> funcsAndIO
  <> section "The universe as part of our program" <> universe 
  <> section "Pure and impure" <> pureAndImpure
  <> section "Functional and imperative" <> funcAndImp
  <> section "I/O in the libraries" <> librariesIO
  <> section "monadic control structures" <> monadicCtrlStr




funcsAndIO :: LaTeXC l => l
funcsAndIO =
  let
    aux1 = item Nothing <> "Ask the user to insert a string"
    aux2 = item Nothing <> "Read their string"
    aux3 = item Nothing <> "Use " <> fbox "fmap" <> " to apply a function " <> fbox "shout" <> " that capitalizes all the letters from the string"
    aux4 = item Nothing <> "Write the resulting string"
  in
    "Let's combine functions with I/O to create a full program that will:"
    <> enumerate (aux1 <> aux2 <> aux3 <> aux4)
    <> fullProgram
    <> "We have a full-blown program, but we didn't include any type definitions. Which parts are functions and which are IO actions or other values? We can load our program in GHCi and check the types:"
    <> types
    <> par <> "Whew, that is a lot of information there. We've seen all of this before, but let's review."
    <> par <> fbox "main" <> " is " <> fbox "IO ()" <> ". That's not a function. Functions are of types " <> fbox (math (" a " <> to <> " b ") ) <> ". Our entire program is an IO action."
    <> par <> fbox "putStrLn" <> " is a function, but it results in an IO action. The " <> qts "Write your string:" <> " text is a " <> fbox "String" <> " (remember, that's just a synonym for " <> fbox "[Char]" 
    <> "). It is used as an argument for " <> fbox "putStrLn" <> " and is incorporated into the IO action that results. So, " <> fbox "putStrLn" <> " is a function, but " 
    <> fbox "putStrLn x" <> " evaluates to an IO action. The " <> fbox "()" <> " part of the IO type indicates that nothing is available to be passed on to any later functions or actions."
    <> "That last part is key. We sometimes say informally that an IO action " <> qts "returns" <> " something; however, taking that too literally leads to confusion. It is clear what we mean when we talk about "
    <> textit "functions" <> " returning results, but IO actions are not functions. Let's skip down to " <> fbox "getLine" <> " - an IO action that " <> textit "does" <> " provide a value. " <> fbox "getLine" <> " is not a function that returns a " 
    <> fbox "String" <> " because " <> fbox "getLine" <> textit " isn't a function" <> ". Rather, " <> fbox "getLine" <> " is an IO action which, when evaluated, will materialize a " 
    <> fbox "String" <> ", which can then be passed to later functions through, for instance, " <> fbox "fmap" <> " and " <> bind <> "."
    <> "When we use " <> fbox "getLine" <> " to get a " <> fbox "String" <> ", the value is monadic because it is wrapped in " <> fbox "IO" <> " functor (which happens to be a monad). We cannot pass the value directly to a function that takes plain (non-monadic, or non-functorial) values. "
    <> fbox "fmap" <> " does the work of taking a non-monadic function while passing in and returning monadic values. "
    <> par <> "As we've seen already, " <> bind <> " does the work of passing a monadic value into a function that takes a non-monadic value and returns a monadic value. It may seem inefficient for " <> fbox "fmap" 
    <> " to take the non-monadic result of its given function and return a monadic value only for " <> bind <> " to then pass the underlying non-monadic value to the next function. It is precisely this sort of chaining"
    <> ", however, that creates the reliable sequencing that make monads so effective at integrating pure functions with IO actions. "
    <> subsubsection (textit "do" <> " notation review")
    <> "Given the emphasis on sequencing, the " <> fbox "do" <> " notation can be especially appealing with the " <> fbox "IO" <> " monad. Our program "
    <> verbatim (pack ( "  putStrLn "<>[c]<>"Write your string: "<>[c]<>" >> fmap shout getLine >>= putStrLn \n " ))
    <> "could be written as:"
    <> fullProgDO




universe :: LaTeXC l => l
universe =
  "One way of viewing the " <> fbox "IO" <> " monad is to consider " <> fbox "IO a" <> " as a computation which provides a value of type " <> fbox "a" <> " while changing " <> textit "the state of the world" 
  <> " by doing input and output. Obviously, you cannot literally set the state of the world; it is hidden from you, as the " <> fbox "IO" <> " functor is abstract (that is, you cannot dig into it to see the underlying values; it is closed in a way opposite to that in which "
  <> fbox "Maybe" <> " can be said to be open). Seen this way, " <> fbox "IO" <> " is roughly analogous to the " <> fbox "State" <> " monad, which we will meet shortly. With " <> fbox "State" <> ", however, the state being changed is made of normal Haskell values, and so we can manipulate it directly with pure functions."
  <> par <> "Understand that this idea of the universe as an object affected and affecting Haskell values through " <> fbox "IO" <> " is only a metaphor; a loose interpretation at best. The more mundane fact is that " <> fbox "IO" <> " simply brings some very base-level operations into the Haskell language."
  <> footnote ( "The technical term is " <> qts "primitive" <> ", as in primitive operations. " )
  <> " Remember that Haskell is an abstraction, and that Haskell programs must be compiled to machine code in order to actually run. The actual workings of IO happen at a lower level of abstraction, and are wired into the very definition of the Haskell language."
  <> footnote ( "The same can be said about all higher-level programming languages, of course. Incidentally, Haskell's IO operations can actually be extended via the " <> textit "Foreign Function Interface" <> " (FFI) which can make calls to C libraries. As C can use inline assembly code, Haskell can indirectly engage with anything a computer can do. Still, Haskell functions manipulate such outside operations only " <> textit "indirectly" <> " as values in IO functors." )
  



pureAndImpure :: LaTeXC l => l
pureAndImpure =
  "Consider the following snippet:"
  <> speakTo
  <> "In most other programming languages, which do not have separate types for I/O actions, " <> fbox "speakTo" <> " would have a type akin to:"
  <> speakToType
  <> "With such a type, however, " <> fbox "speakTo" <> " would not be a function at all! Functions produce the same results when given the same arguments; the " <> fbox "String" <> " delivered by " <> fbox "speakTo"
  <> ", however, also depends on whatever is typed at the terminal prompt. In Haskell, we avoid that pitfall by returning an " <> fbox "IO String" <> ", which is not a "
  <> fbox "String" <> " but a promise that " <> textit "some " <> fbox "String" <> " will be delivered by carrying out certain instructions involving I/O (in this case, the I/O consists of getting a line of input from the terminal)."
  <> " Though the " <> fbox "String" <> " can be different each time " <> fbox "speakTo" <> " is evaluated, the I/O instructions are always the same."
  <> par <> "When we say Haskell is a purely functional language, we mean that all of its functions are " <> textit "really" <> " functions, which is not the case in most other languages. To be precise, Haskell expressions are always "
  <> textit "referentially transparent" <> "; that is, you can always replace an expression (such as " <> fbox "speakTo" <> ") with its value (in this case, " <> fbox "\\fSentence -> fmap fSentence getLine" <> ") without changing the behaviour of the program. The "
  <> fbox "String" <> " delivered by " <> fbox "getLine" <> ", in contrast, is opaque; its value is not specified and can't be discovered in advance by the program. If "
  <> fbox "speakTo" <> " had the problematic type we mentioned above, " <> fbox "sayHello" <> " would be a " <> fbox "String" <> "; however, replacing it by any specific string would break the program."
  <> par <> "In spite of Haskell being purely functional, " <> fbox "IO" <> " actions can be said to be " <> textit "impure" <> " because their impact on the outside world are " <> textit "side effects" <> " (as opposed to the regular effects that are entirely contained within Haskell)"
  <> ". Programming languages that lack purity may have side-effects in many other places connected with various calculations. Purely functional languages, however, assure that " <> textit "even expressions with impure values are referentially transparent" 
  <> ". That means we can talk about, reason about and handle impurity in a purely functional way, using purely functional machinery such as functors and monads. While " <> fbox "IO" <> " actions are impure, all of the Haskell functions that manipulate them remain pure."
  <> par <> "Functional purity, coupled to the fact that I/O shows up in types, benefit Haskell programmers in various ways. The guarantees about referential transparency increase a lot the potential for compiler optimizations. "
  <> fbox "IO" <> " values being distinguishable through types alone make it possible to immediately tell where we are engaging with side effects or opaque values. As " <> fbox "IO" <> " itself is just another functor, we maintain to the fullest extent the predictability and ease of reasoning associated with pure functions."




funcAndImp :: LaTeXC l => l
funcAndImp =
  "When we introduced monads, we said that a monadic expression can be interpreted as a statement of an imperative language. That interpretation is immediately compelling for " <> fbox "IO" <> ", as the language around IO actions looks a lot like a conventional imperative language. It must be clear, however, that we are talking about an " <> textit "interpretation" 
  <> ". We are not saying that monads or " <> fbox "do" <> " notation turn Haskell into an imperative language. The point is merely that you can view and understand monadic code in terms of imperative statements. The semantics may be imperative, but the implementation of monads and " <> bind <> " is still purely functional. To make this distinction clear, let's look at a little illustration:"
  <> snippet1
  <> "This is a snippet of C, a typical imperative language. In it, we declare a variable " <> fbox "x" <> ", read its value from user input with " <> fbox "scanf" <> " and then print it with " <> fbox "printf"
  <> ". We can, within an " <> fbox "IO" <> " do block, write a Haskell snippet that performs the same function and looks quite similar:"
  <> snippet2
  <> "Semantically, the snippets are nearly equivalent."
  <> footnote ( "One difference is that " <> fbox "x" <> " is a mutable variable in C, and so it is possible to declare it in one statement and set its value in the next; Haskell never allows such mutability. If we wanted to imitate the C code even more closely, we could have used an " <> fbox "IORef" <> ", which is a cell that contains a value which can be destructively updated. For obvious reasons, " <> fbox "IORefs" <> " can only be used within the " <> fbox "IO" <> " monad." )
  <> "  In the C code, however, the statements directly correspond to instructions to be carried out by the program. The Haskell snippet, on the other hand, is desugared to:"
  <> snippet3
  <> "The desugared version has no statements, only functions being applied. We tell the program the order of the operations indirectly as a simple consequence of " <> textit "data dependencies" <> ": when we chain monadic computations with " <> bind <> ", we get the later results by applying functions to the results of the earlier ones. It just happens that, for instance, evaluating "
  <> fbox "print x" <> " leads to a string to be printed in the terminal."
  <> par <> "When using monads, Haskell allows us to write code with imperative semantics while keeping the advantages of functional programming."




librariesIO :: LaTeXC l => l
librariesIO =
  "So far the only I/O primitives we have used were " <> fbox "putStrLn" <> " and " <> fbox "getLine" <> " and small variations thereof. The standard libraries, however, offer many other useful functions and actions involving "
  <> fbox "IO" <> ". We present some of the most important ones in the next appendix, including the basic functionality needed for reading from and writing to files."




monadicCtrlStr :: LaTeXC l => l
monadicCtrlStr =
  "Given that monads allow us to express sequential execution of actions in a wholly general way, could we use them to implement common iterative patterns, such as loops? In this section, we will present a few of the functions from the standard libraries which allow us to do precisely that. While the examples are presented here applied to " 
  <> fbox "IO" <> ", keep in mind that the following ideas apply to " <> textit "every" <> " monad."
  <> par <> "Remember, there is nothing magical about monadic values; we can manipulate them just like any other values in Haskell. Knowing that, we might think to try the following function to get five lines of user input:"
  <> verbatim (pack "  fiveGetLines = replicate 5 getLine \n " )
  <> "That won't do, however (try it in GHCi!). The problem is that " <> fbox "replicate" <> " produces, in this case, a list of actions, while we want an action which returns a list (that is, " <> fbox "IO [String]" <> " rather than " <> fbox "[IO String]" 
  <> "). What we need is a " <> textit "fold" <> " to run down the list of actions, executing them and combining the results into a single list. As it happens, there is a Prelude function which does that: " <> fbox "sequence" <> "."
  <> verbatim (pack "  sequence :: (Monad m) => [m a] -> m [a] \n " )
  <> "And so, we get the desired action with:"
  <> verbatim (pack " fiveGetLines = sequence $ replicate 5 getLine \n " )
  <> fbox "replicate" <> " and " <> fbox "sequence" <> " form an appealing combination; so " <> fbox "Control.Monad" <> " offers a " <> fbox "replicateM" <> " function for repeating an action an arbitrary number of times. "
  <> fbox "Control.Monad" <> " provides a number of other convenience functions in the same spirit - monadic zips, folds, and so on."
  <> verbatim (pack " fiveGetLinesAlt = replicateM 5 getLine \n " )
  <> "A particularly important combination is " <> fbox "map" <> " and " <> fbox "sequence" <> ". Together, they allow us to make actions from a list of values, run them sequentially, and collect the results. " <> fbox "mapM" <> ", a Prelude function, captures this pattern:"
  <> verbatim (pack " mapM :: (Monad m) => (a -> m b) -> [a] -> m [b] \n " )
  <> "We also have variants of the above functions with a trailing underscore in the name, such as " <> fbox "sequence_" <> ", " <> fbox "mapM_" <> " and " <> fbox "replicateM_"
  <> ". These discard any final values and so are appropriate when you are only interested in performing actions. Compared with their underscore-less counterparts, these functions are like the distinction between "
  <> bind <> " and " <> scriptsize (fbox (math "(>>=)")) <> ". " <> fbox "mapM_" <> " for instance has the following type:"
  <> verbatim (pack " mapM_ :: (Monad m) => (a -> m b) -> [a] -> m () \n " )
  <> "Finally, it is worth mentioning that " <> fbox "Control.Monad" <> " also provides " <> fbox "forM" <> " and " <> fbox "forM_" <> ", which are flipped versions of " <> fbox "mapM" <> " and " <> fbox "mapM_"
  <> ". " <> fbox "forM_" <> " happens to be the idiomatic Haskell counterpart to the imperative for-each loop; and the type signature suggests that neatly:"
  <> verbatim (pack " forM_ :: (Monad m) => [a] -> (a -> m b) -> m () \n " )




