{-# LANGUAGE OverloadedStrings #-}

module TFG_additiveMonads where



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


monadPlusMaybe :: LaTeXC l => l
monadPlusMaybe = format t where
  t =
    "  instance MonadPlus Maybe where                                                        \n"
    <> "    mzero                   = Nothing                                                \n"
    <> "    Nothing `mplus` Nothing = Nothing -- 0 solutions + 0 solutions = 0 solutions     \n"
    <> "    Just x  `mplus` Nothing = Just x  -- 1 solution  + 0 solutions = 1 solution      \n"
    <> "    Nothing `mplus` Just x  = Just x  -- 0 solutions + 1 solution  = 1 solution      \n"
    <> "    Just x  `mplus` Just y  = Just x  -- 1 solution  + 1 solution  = 2 solutions,    \n"
    <> "                                      -- but Maybe can only have up to one solution, \n"
    <> "                                      -- so we disregard the second one.             \n"

eitherMonadPlus :: LaTeXC l => l
eitherMonadPlus = format t where
  t =
    "  instance (Error e) => MonadPlus (Either e) where   \n"
    <> "    mzero             = Left noMsg                \n"
    <> "    Left _  `mplus` n = n                         \n"
    <> "    Right x `mplus` _ = Right x                   \n"

digit :: LaTeXC l => l
digit = format t where
  t =
    "  digit :: Int -> String -> Maybe Int             \n"
    <> "  digit i s | i > 9 || i < 0 = Nothing         \n"
    <> "            | otherwise      = do              \n"
    <> "    let (c:_) = s                              \n"
    <> "    if [c] == show i then Just i else Nothing  \n"
 
mpLawsCode :: LaTeXC l => l
mpLawsCode = format t where
  t =
    "  -- mzero is a neutral element \n"
    <> "  mzero `mplus` m  =  m \n"
    <> "  m `mplus` mzero  =  m \n"
    <> "  -- mplus is associative \n"
    <> "  -- (but not all instances obey this law because it makes some infinite structures impossible) \n"
    <> "  m `mplus` (n `mplus` o)  =  (m `mplus` n) `mplus` o \n"

ternasPit :: LaTeXC l => l
ternasPit = format t where
  t =
    "  pythags = do                   \n"
    <> "    z <- [1..]                \n"
    <> "    x <- [1..z]               \n"
    <> "    y <- [x..z]               \n"
    <> "    guard (x^2 + y^2 == z^2)  \n"
    <> "    return (x, y, z)          \n"

pythags :: LaTeXC l => l
pythags = format t where
  t =
    "  pythags =                                \n"
    <> "    [1..] >>= \\z ->                     \n"
    <> "    [1..z] >>= \\x ->                    \n"
    <> "    [x..z] >>= \\y ->                    \n"
    <> "    guard (x^2 + y^2 == z^2) >>= \\_ ->  \n"
    <> "    return (x, y, z)                    \n"

pythagsExtended :: LaTeXC l => l
pythagsExtended = format t where
  t =
    "  pythags =                                                                    \n"
    <> "   let ret x y z = [(x, y, z)]                                              \n"
    <> "       gd  z x y = concatMap (\\_ -> ret x y z) (guard $ x^2 + y^2 == z^2)  \n"
    <> "       doY z x   = concatMap (gd  z x) [x..z]                               \n"
    <> "       doX z     = concatMap (doY z  ) [1..z]                               \n"
    <> "       doZ       = concatMap (doX    ) [1..]                                \n"
    <> "   in doZ                                                                   \n"
 
pythagsTree :: LaTeXC l => l
pythagsTree = format t where
  t =
    "  start                                                                    \n"
    <> "     |____________________________________________ ...                  \n"
    <> "     |                     |                    |                       \n"
    <> "  x  1                     2                    3                       \n"
    <> "     |_______________ ...  |_______________ ... |_______________ ...    \n"
    <> "     |      |      |       |      |      |      |      |      |         \n"
    <> "  y  1      2      3       2      3      4      3      4      5         \n"
    <> "     |___...|___...|___... |___...|___...|___...|___...|___...|___...   \n"
    <> "     | | |  | | |  | | |   | | |  | | |  | | |  | | |  | | |  | | |     \n"
    <> "  z  1 2 3  2 3 4  3 4 5   2 3 4  3 4 5  4 5 6  3 4 5  4 5 6  5 6 7     \n"


----------------------------------------




addMonads :: LaTeXC l => l
addMonads = 
  "In our studies so far, we saw that the " <> fbox "Maybe" <> " and list monads both represent the number of results a computation can have. That is, you use " 
  <> fbox "Maybe" <> " when you want to indicate that a computation can fail somehow (i.e. it can have 0 results or 1 result), and you use the list monad when you want to indicate a computation could have many valid answers ranging from 0 results to many results."
  <> par <> "Given two computations in one of these monads, it might be interesting to amalgamate " <> textit "all" <> " valid solutions into a single result. For example, within the list monad, we can concatenate two lists of valid solutions."
  <> subsection ( textit "MonadPlus" <> " definition") <> monadPlusDef
  <> subsection "Example: parallel parsing" <> ejParsing
  <> subsection "The MonadPlus laws" <> monadPlusLaws
  <> subsection "Useful functions" <> usefulFuncs
  <> subsection "Relationship with monoids" <> monoidsPlus



monadPlusDef :: LaTeXC l => l
monadPlusDef =
  fbox "MonadPlus" <> " defines two methods. " <> fbox "mzero" <> " is the monadic value standing for zero results; while " <> fbox "mplus" <> " is a binary function which combines two computations."
  <> format "  class Monad m => MonadPlus m where   \n    mzero :: m a   \n    mplus :: m a -> m a -> m a   \n"
  <> "Here are the two instance declarations for " <> fbox "Maybe" <> " and the list monad:"
  <> format "  instance MonadPlus [] where   \n    mzero = []   \n    mplus = (++)   \n \n"
  <> monadPlusMaybe
  <> "Also, if you import " <> fbox "Control.Monad.Error" <> ", then " <> fbox "(Either e)" <> " becomes an instance:"
  <> eitherMonadPlus
  <> "Like " <> fbox "Maybe" <> ", " <> fbox "(Either e)" <> " represents computations that can fail. Unlike " <> fbox "Maybe" <> ", " <> fbox "(Either e)" 
  <> " allows the failing computations to include an error " <> qts "message" <> " (which is usually a " <> fbox "String" <> "). Typically, " <> fbox "Left s" 
  <> " means a failed computation carrying an error message " <> fbox "s" <> ", and " <> fbox "Right x" <> " means a successful computation with result " <> fbox "x" <> "."




ejParsing :: LaTeXC l => l
ejParsing =
  "Traditional input parsing involves functions which consume an input one character at a time. That is, a parsing function takes an input string and chops off (i.e. 'consumes') characters from the front if they satisfy certain criteria"
  <> ". For example, you could write a function which consumes one uppercase character. If the characters on the front of the string don't satisfy the given criteria, the parser has " <> textit "failed" <> "; so such functions are candidates for " <> fbox "Maybe" <> "."
  <> par <> "Let's use " <> fbox "mplus" <> " to run two parsers " <> textit "in parallel" <> ". That is, we use the result of the first one if it succeeds, and otherwise, we use the result of the second. If both fail, then our whole parser returns " <> fbox "Nothing" <> "."
  <> par <> "In the example below, we consume a digit in the input and return the digit that was parsed."
  <> digit
  <> "Our guards assure that the " <> fbox "Int" <> " we are checking for is a single digit. Otherwise, we are just checking that the first character of our String matches the digit we are checking for. If it passes, we return the digit wrapped in a " <> fbox "Just"
  <> ". The do-block assures that any failed pattern match will result in returning " <> fbox "Nothing" <> "."
  <> par <> "We can use our digit function with mplus to parse Strings of " <> fbox "binary" <> " digits:"
  <> format "  binChar :: String -> Maybe Int   \n  binChar s = digit 0 s `mplus` digit 1 s   \n"
  <> "Parser libraries often make use of " <> fbox "MonadPlus" <> " in this way. If you are curious, check the " <> fbox "(+++)" <> " operator in " <> fbox "Text.ParserCombinators.ReadP" <> ", or " <> fbox (scriptsize (math "(<|>)" )) <> " in " <> fbox "Text.ParserCombinators.Parsec.Prim."




monadPlusLaws :: LaTeXC l => l
monadPlusLaws =
  "Instances of MonadPlus are required to fulfill several rules, just as instances of Monad are required to fulfill the three monad laws. Unfortunately, the MonadPlus laws aren't fully agreed on. The most common approach says that mzero and mplus form a " <> textit "monoid" <> ". By that, we mean:"
  <> mpLawsCode 
  <> "The Haddock documentation for " <> fbox "Control.Monad" <> " quotes additional laws:"
  <> format "  mzero >>= f  =  mzero   \n  m >> mzero   =  mzero   \n"
  <> "And the HaskellWiki page cites another (with controversy):"
  <> format "  (m `mplus` n) >>= k   =   (m >>= k) `mplus` (n >>= k)   \n"
  <> "There are even more sets of laws available. Sometimes monads like IO are used as a MonadPlus. Consult All About Monads and the Haskell Wiki page on MonadPlus for more information about such issues."




usefulFuncs :: LaTeXC l => l
usefulFuncs =
  "Beyond the basic " <> fbox "mplus" <> " and " <> fbox "mzero" <> ", there are two other general-purpose functions involving " <> fbox "MonadPlus" <> ":"
  <> subsubsection "msum"
  <> "A common task when working with MonadPlus: take a list of monadic values, e.g. " <> fbox "[Maybe a]" <> " or " <> fbox "[[a]]" <> ", and fold it down with mplus. The function msum fulfills this role:"
  <> format "  msum :: MonadPlus m => [m a] -> m a   \n  msum = foldr mplus mzero   \n"
  <> "In a sense, " <> fbox "msum" <> " generalizes the list-specific " <> fbox "concat" <> " operation. Indeed, the two are equivalent when working on lists. For Maybe, " <> fbox "msum" <> " finds the first " <> fbox "Just x" <> " in the list and returns " <> fbox "Nothing" <> " if there aren't any."
  <> subsubsection "guard"
  <> "When discussing the list monad we note how similar it is to list comprehensions, but we didn't discuss how to mirror list comprehension filtering. The " <> fbox "guard" <> " function allows us to do exactly that."
  <> par <> "Consider the following comprehension which retrieves all pythagorean triples (i.e. trios of integer numbers which work as the lengths of the sides for a right triangle). First we'll examine the brute-force approach. We'll use a boolean condition for filtering; namely, Pythagoras' theorem:"
  <> format "  pythags = [ (x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2 ]  \n"
  <> "The translation of the comprehension above to the list monad is:"
  <> ternasPit
  <> "The " <> fbox "guard" <> " function works like this:"
  <> format "  guard :: MonadPlus m => Bool -> m ()   \n  guard True  = return ()   \n  guard _ = mzero   \n"
  <> "Concretely, " <> fbox "guard" <> " will reduce a do-block to " <> fbox "mzero" <> " if its predicate is " <> fbox "False" <> ". Given the first law stated in the 'MonadPlus laws' section above, an "
  <> fbox "mzero" <> " on the left-hand side of a " <> bind <> " operation will produce " <> fbox "mzero" <> " again. As do-blocks are decomposed to lots of expressions joined up by " <> bind
  <> ", an " <> fbox "mzero" <> " at any point will cause the entire do-block to become " <> fbox "mzero" <> "."
  <> par <> "To further illustrate, we will examine " <> fbox "guard" <> " in the special case of the list monad, extending on the " <> fbox "pythags" <> " function above. First, here is " <> fbox "guard" <> " defined for the list monad:"
  <> format "  guard :: Bool -> [()]   \n  guard True  = [()]   \n  guard _ = []   \n"
  <> "Basically, " <> fbox "guard" <> textit " blocks off" <> " a route. In " <> fbox "pythags" <> ", we want to block off all the routes (or combinations of "<> fbox "x" <> ", " <> fbox "y" <> " and " <> fbox "z" 
  <> ") where " <> fbox ( "x^2 + y^2 == z^2" ) <> " is " <> fbox "False" <> ". Let's look at the expansion of the above " <> fbox "do" <> "-block to see how it works:"
  <> pythags
  <> "Replacing " <> bind <> " and " <> fbox "return" <> " with their definitions for the list monad (and using some let-bindings to keep it readable), we obtain:"
  <> pythagsExtended
  <> "Remember that " <> fbox "guard" <> " returns the empty list in the case of its argument being " <> fbox "False" <> ". Mapping across the empty list produces the empty list, no matter what function you pass in. So the empty list produced by the call to "
  <> fbox "guard" <> " in the binding of " <> fbox "gd" <> " will cause " <> fbox "gd" <> " to be the empty list, and therefore " <> fbox "ret" <> " to be the empty list."
  <> par <> "To understand why this matters, think about list-computations as a tree. With our Pythagorean triple algorithm, we need a branch starting from the top for every choice of " <> fbox "z" 
  <> ", then a branch from each of these branches for every value of " <> fbox "x" <> ", then from each of these, a branch for every value of " <> fbox "y" <> ". So the tree looks like this:"
  <> pythagsTree
  <> "Each combination of x, y and z represents a route through the tree. Once all the functions have been applied, each branch is concatenated together, starting from the bottom. Any route where our predicate doesn't hold evaluates to an empty list, and so has no impact on this concat operation."
  



monoidsPlus :: LaTeXC l => l
monoidsPlus =
  "When discussing the MonadPlus laws, we alluded to the mathematical concept of monoids. It turns out that there is a " <> fbox "Monoid" 
  <> " class in Haskell, defined in " <> fbox "Data.Monoid" <> ". A fuller presentation of is given in an appendix. For now, a minimal definition of "
  <> fbox "Monoid" <> " implements two methods; namely, a neutral element (or 'zero') and an associative binary operation (or 'plus')."
  <> format "  class Monoid m where   \n    mempty  :: m   \n    mappend :: m -> m -> m   \n"
  <> "For example, lists form a simple monoid:"
  <> format "  instance Monoid [a] where   \n    mempty  = []   \n    mappend = (++)   \n"
  <> "Sounds familiar, doesn't it? In spite of the uncanny resemblance to " <> fbox "MonadPlus" <> ", there is a subtle yet key difference. Note the usage of " <> fbox "[a]" <> " instead of " <> fbox "[ ]" <> " in the instance declaration. Monoids are not necessarily "
  <> qts "containers" <> " of anything or parametrically polymorphic. For instance, the integer numbers on form a monoid under addition with 0 as neutral element."
  <> par <> "In any case, " <> fbox "MonadPlus" <> " instances look very similar to monoids, as both feature concepts of zero and plus. Indeed, we could even make " <> fbox "MonadPlus" <> " a subclass of " <> fbox "Monoid" <> " if it were worth the trouble:"
  <> format "  instance MonadPlus m => Monoid (m a) where   \n    mempty  = mzero   \n    mappend = mplus   \n"
  <> paragraph "Note" <> "Due to the " <> qts "free" <> " type variable " <> fbox "a" <> " in the instance definition, the snippet above is not valid Haskell 98. If you want to test it, you will have to enable the GHC "
  <> textit "language extension " <> fbox "FlexibleInstances: " <> itemize (aux1 <> aux2)
  <> par <> "Again, " <> fbox "Monoids" <> " and " <> fbox "MonadPlus" <> " work at different levels. As noted before, there is no requirement for monoids to be parameterized in relation to "
  <> qts "contained" <> " or related type. More formally, monoids have kind " <> math "*" <> ", but instances of " <> fbox "MonadPlus" <> " (which are monads) have kind " <> math "* -> *" <> "."
    where
      aux1 = item Nothing <> "If you are testing with GHCi, start it with the command line option " <> verb "-XFlexibleInstances" <> " or interactively type " <> fbox ":set -XFlexibleInstances."
      aux2 = item Nothing <> "Alternatively, if you are running a compiled program, add " <> verb "{-# LANGUAGE FlexibleInstances #-}" <> " to the top of your source file."














