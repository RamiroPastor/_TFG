{-# LANGUAGE OverloadedStrings #-}

module TFG_listMonad where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath

import TFG_maybeMonad (bind)





-------- SIMBOLOS U OTRO CODIGO --------


bunnyExample :: LaTeXC l => l
bunnyExample = verbatim (pack t) where
  t =
    "  Prelude> let generation = replicate 2             \n"
    <> "  Prelude> ['bunny'] >>= generation                  \n"
    <> "  ['bunny','bunny']                                  \n"
    <> "  Prelude> ['bunny'] >>= generation >>= generation   \n"
    <> "  ['bunny','bunny','bunny','bunny']                  \n"


bg1 :: LaTeXC l => l
bg1 = verbatim (pack t) where
  t =
    "  nextConfigs :: Board -> [Board]                     \n"
    <> "  nextConfigs bd = undefined -- details not important  \n"

bg2 :: LaTeXC l => l
bg2 = verbatim ( pack "  nextConfigs bd >>= nextConfigs" )

bg3 :: LaTeXC l => l
bg3 = verbatim (pack t) where
  t =
    "  threeTurns :: Board -> [Board]  \n"
    <> "  threeTurns bd = do           \n"
    <> "    bd1 <- nextConfigs bd      \n"
    <> "    bd2 <- nextConfigs bd1     \n"
    <> "    nextConfigs bd2            \n"

threeTurnsDO :: LaTeXC l => l
threeTurnsDO = verbatim (pack t) where
  t =
    "  threeTurns bd = do          \n"
    <> "    bd1 <- nextConfigs bd  \n"
    <> "    bd2 <- nextConfigs bd1 \n"
    <> "    bd3 <- nextConfigs bd2 \n"
    <> "    return bd3             \n"

listCompEx :: LaTeXC l => l
listCompEx = verbatim ( pack t ) where
  t =
    "threeTurns bd = \n  [ bd3 | bd1 <- nextConfigs bd, bd2 <- nextConfigs bd1, bd3 <- nextConfigs bd2 ]"

----------------------------------------




anexo5 :: LaTeXC l => l
anexo5 =
  chapter "Appendix: The List monad"
  <> "Lists are a fundamental part of Haskell, and we've used them extensively before getting to this chapter. The novel insight is that the list type is a monad too!"
  <> par <> "As monads, lists are used to model " <> textit "nondeterministic" <> " computations which may return an arbitrary number of results."
  <> " There is a certain parallel with how " <> fbox "Maybe" <> " represented computations which could return zero or one value"
  <> "; but with lists, we can return zero, one, or many values (the number of values being reflected in the length of the list)."
  <> section "List instantiated as monad" <> listMonad
  <> section "Board game example" <> bgExample
  <> section "List comprehensions" <> listComp




listMonad :: LaTeXC l => l
listMonad =
  "The " <> fbox "return" <> " function for lists simply injects a value into a list:"
  <> verbatim ( pack "  return x  =  [x] \n" )
  <> "In other words, " <> fbox "return" <> " here makes a list containing one element, namely the single argument it took. The type of the " <> textit "list return" <> " is " <> fbox ( "return :: " <> math ( "a " <> to <> " [a]")) 
  <> ", or, equivalently, " <> fbox ( "return :: " <> math ( "a "<>to<> "["<>(raw "\\,")<>"]"<>(raw "\\:")<>"a")) <> ". The latter style of writing it makes it more obvious that we are replacing the generic type constructor (which we had called "
  <> fbox "M" <> " in Understanding monads) by the list type constructor " <> fbox "[ ]" <> " (which is distinct from but easy to confuse with the empty list!)."
  <> par <> "The binding operator is less trivial. We will begin by considering its type, which for the case of lists should be:"
  <> verbatim ( pack "  [a] -> (a -> [b]) -> [b] \n" )
  <> "This is just what we'd expect: it pulls out the value from the list to give to a function that returns a new list."
  <> par <> "The actual process here involves first mapping a given function over a given list to get back a list of lists, i.e. type "  
  <> fbox "[[b]]" <> " (of course, many functions which you might use in mapping do not return lists; but, as shown in the type signature above, " <> textbf "monadic binding for lists only works with functions that return lists" 
  <> "). To get back to a regular list, we then concatenate the elements of our list of lists to get a final result of type " <> fbox "[b]" <> ". Thus, we can define the list version of " <> bind <> ":"
  <> verbatim ( pack "  xs >>= f = concat (map f xs) \n" )
  <> "The bind operator is key to understanding how different monads do their jobs, and its definition indicates the chaining strategy for working with the monad."
  <> par <> "For the list monad, non-determinism is present because different functions may return any number of different results when mapped over lists."
  <> subsubsection "Bunny invasion"
  <> bunnyExample
  <> "In this silly example all elements are equal, but the same overall logic could be used to model radioactive decay, or chemical reactions, or any phenomena that produces a series of elements starting from a single one."




bgExample :: LaTeXC l => l
bgExample =
  "Suppose we are modeling a turn-based board game and want to find all the possible ways the game could progress. We would need a function to calculate the list of options for the next turn, given a current board state:"
  <> bg1
  <> "To figure out all the possibilities after two turns, we would again apply our function to each of the elements of our new list of board states"
  <> ". Our function takes a single board state and returns a list of possible new states. Thus, we can use monadic binding to map the function over each element from the list:"
  <> bg2
  <> "In the same fashion, we could bind the result back to the function yet again (ad infinitum) to generate the next turn's possibilities. Depending on the particular game's rules, we may reach board states that have no possible next-turns; in those cases, our function will return the empty list."
  <> par <> "On a side note, we could translate several turns into a do block (like we did for the grandparents example in Understanding monads):"
  <> bg3
  <> "If the above looks too magical, keep in mind that " <> fbox "do" <> " notation is syntactic sugar for " <> bind <> " operations. To the right of each left-arrow, there is a function with arguments that evaluate to a list; the variable to the left of the arrow stands for the list elements"
  <> ". \n After a left-arrow assignment line, there can be later lines that call the assigned variable as an argument for a function. This later function will be performed for " <> textit "each" <> " of the elements from within the list that came from the left-arrow line's function. This per-element process corresponds to the `map' in the definition of " 
  <> bind <> ". A resulting list of lists (one per element of the original list) will be flattened into a single list (the `concat' in the definition of " <> bind <> ")."




listComp :: LaTeXC l => l
listComp =
  "The list monad works in a way that has uncanny similarity to list comprehensions. Let's slightly modify the " <> fbox "do" <> " block we just wrote for " <> fbox "threeTurns" <> " so that it ends with a " <> fbox "return" <> "..."
  <> threeTurnsDO
  <> "This mirrors exactly the following list comprehension:"
  <> listCompEx
  <> "(In a list comprehension, it is perfectly legal to use the elements drawn from one list to define the following ones, like we did here.)"
  <> par <> "The resemblance is no coincidence: list comprehensions are, behind the scenes, defined in terms of " <> fbox "concatMap"
  <> verbatim ( pack "  concatMap f xs = concat (map f xs)  " ) 
  <> ". That's just the list monad binding definition again! To summarize the nature of the list monad: binding for the list monad is a combination of concatenation and mapping, and so the combined function " 
  <> fbox "concatMap" <> " is effectively the same as " <> bind <> " for lists (except for different syntactic order)."
  <> par <> "For the correspondence between list monad and list comprehension to be complete, we need a way to reproduce the filtering that list comprehensions can do. Search for Additive Monads (MonadPlus)."



