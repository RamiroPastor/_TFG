{-# LANGUAGE OverloadedStrings #-}




module TFG_ejercicios where





import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath


import TFG_maybeMonad (maybeMonad, bind)
import TFG_monadasHaskell (appMaybe, apply)




-------- SIMBOLOS U OTRO CODIGO --------

format :: LaTeXC l => String -> l
format = verbatim . pack

funcType :: LaTeXC l => l
funcType = fbox ( math ( "((" <> to <> ")"<>(raw "\\;")<>"r)" ) )

formatSymbol :: LaTeXC l => l -> l
formatSymbol = scriptsize . fbox . math


----------------------------------------






enunciados :: LaTeXC l => l
enunciados = 
  chapter "Exercises" 
  <> "These exercises have been taken from several different sources, and are not necessarily sorted by any criteria"
  <> section ( "Basic " <> textit "Functor" <> " and " <> textit "Applicative" <> " exercises")
  <> enumerate (ejercicio1 <> ejercicio2 <> ejercicio3)
  <> newpage  
  <> section ( "Advanced " <> textit "Monad" <> " and " <> textit "Applicative" <> " exercises" )
  <> enumerate (ejercicio4 <> ejercicio5 <> ejercicio6 <> ejercicio7 <> ejercicio8 <> ejercicio9 <> ejercicio10 <> ejercicio11 <> ejercicio12)
  <> par <> "The next few exercises concern the following tree data structure:"
  <> footnote ( "In case you are wondering, " <> qts "AT" <> " stands for " <> qts "apple tree" <> "." ) 
  <> verbatim "  data AT a = L a | B (AT a) (AT a)  \n"
  <> enumerate ("[resume]" <> ejercicio13 <> ejercicio14 <> ejercicio15 <> ejercicio16 <> ejercicio17 <> ejercicio18)
  <> newpage  
  <> section ( textit "State" <> " exercises" )
  <> enumerate (ejercicio19 <> ejercicio20 <> ejercicio21 <> ejercicio22 <> ejercicio23 <> ejercicio24)
  <> newpage  
  <> section ( textit "MonadPlus" <> " exercises" )
  <> enumerate (ejercicio25 <> ejercicio26)
  <> section "Monad transformers exercises'"
  <> enumerate (ejercicio27 <> ejercicio28 <> ejercicio29 <> ejercicio30)
  <> newpage  
  <> section "Hask category exercises"
  <> enumerate (ejercicio31 <> ejercicio32 <> ejercicio33 <> ejercicio34 <> ejercicio35 <> ejercicio36 )




ejercicio1 :: LaTeXC l => l
ejercicio1 = item Nothing <> "Define instances of " <> fbox "Functor" <> " for the following types: " <> itemize (ej1_1 <> ej1_2 <> ej1_3) 
  where
    ej1_1 = item Nothing <> "A rose tree, defined as: " <> fbox "data Tree a = Node a [Tree a]"
    ej1_2 = item Nothing <> fbox "Either e" <> " for a fixed " <> fbox "e" <> "."
    ej1_3 = item Nothing <> "The function type " <>  fbox ( math ( "((" <> to <> ")"<>(raw "\\;")<>"r)" ) ) <> ". In this case, " <> fbox "f a" <> " will be " <> fbox ( math ( "(r"<>to<>"a)" )  )                                              


ejercicio2 :: LaTeXC l => l
ejercicio2 = item Nothing <> "Check that the Applicative laws hold for the instance for " <> fbox "Maybe" <> " presented in the main body:" <> appMaybe


ejercicio3 :: LaTeXC l => l
ejercicio3 = item Nothing <> "Write " <> fbox "Applicative" <> " instances for " <> itemize (ej3_1 <> ej3_2)
  where
    ej3_1 = item Nothing <> fbox "Either e" <> ", for a fixed " <> fbox "e"
    ej3_2 = item Nothing <> funcType <> ", for a fixed " <> fbox "r"


ejercicio4 :: LaTeXC l => l
ejercicio4 = item Nothing <> "What is the expected behavior of " <> fbox "sequence" <> " for the " <> fbox "Maybe" <> " monad?"


ejercicio5 :: LaTeXC l => l
ejercicio5 = item Nothing <> "Write a definition of " <> apply <> " using " <> bind <> " and " <> fbox "fmap" <> ". Do not use do-notation."


ejercicio6 :: LaTeXC l => l
ejercicio6 = item Nothing <> "Implement" <> format "   liftA5 :: Applicative f => (a -> b -> c -> d -> e -> k)  \n   -> f a -> f b -> f c -> f d -> f e -> f k "


ejercicio7 :: LaTeXC l => l
ejercicio7 = 
  item Nothing <> "For the list functor, implement from scratch (that is, without using anything from " <> fbox "Applicative" <> " or " <> fbox "Monad" 
  <> " directly) both " <> apply <> " and its version with the " <> qts "wrong" <> " sequencing of effects,"
  <> format "   (<|*|>) :: Applicative f => f (a -> b) -> f a -> f b   \n"


ejercicio8 :: LaTeXC l => l
ejercicio8 =
  item Nothing 
  <> label "ej8"
  <> "Rewrite the definition of commutativity for a " <> fbox "Monad" <> ";"
  <> format "   liftA2 f u v = liftA2 (flip f) v u -- Commutativity  \n      -- Or, equivalently,  \n   f <$> u <*> v = flip f <$> v <*> u  \n"
  <> "using do-notation instead of " <> fbox "ap" <> " or " <> fbox "liftM2" <> "."


ejercicio9 :: LaTeXC l => l
ejercicio9 =
  item Nothing <> "Are the following applicative functors commutative?" <> itemize (ej9_1 <> ej9_2 <> ej9_3) <> par <> "Hint: You may find the answer to exercise " <> ref "ej8" <> " (in this section) useful."
    where
      ej9_1 =  item Nothing <> fbox "ZipList"
      ej9_2 = item Nothing <> funcType
      ej9_3 =  item Nothing <> fbox "State s" <> " (Use the " <> fbox "newtype" <> " definition from the " <> fbox "State" <> " appendix)."


ejercicio10 :: LaTeXC l => l
ejercicio10 = item Nothing <> "What is the result of " <> verb "[2,7,8] *> [3,9]" <> "? Try to guess without writing.)"


ejercicio11 :: LaTeXC l => l
ejercicio11 = item Nothing <> "Implement " <> ( scriptsize $ fbox $ math "(<**>)" ) <> " in terms of other " <> fbox "Applicative" <> " functions."


ejercicio12 :: LaTeXC l => l
ejercicio12 = item Nothing <> "As we have just seen, some functors allow two legal implementations of " <> apply <> " which are only different in the sequencing of effects. Why there is not an analogous issue involving " <> bind <> "?"


ejercicio13 :: LaTeXC l => l
ejercicio13 = 
  item Nothing <> "Write " <> fbox "Functor" <> ", " <> fbox "Applicative" <> " and " <> fbox "Monad" <> " instances for " <> fbox "AT" <> ". Do not use shortcuts such as " <> fbox "pure = return" <> ". The " <> fbox "Applicative" <> " and " 
  <> fbox "Monad" <> " instances should match; in particular, " <> bind <> " should be equivalent to " <> fbox "ap" <> ", which follows from the " <> fbox "Monad" <> " instance."


ejercicio14 :: LaTeXC l => l
ejercicio14 =
  item Nothing <> "Implement the following functions, using either the " <> fbox "Applicative" <> " instance, the " <> fbox "Monad" <> " one or neither of them, if neither is enough to provide a solution. Between Applicative and Monad, choose the "
  <> textit "least" <> " powerful one which is still good enough for the task. Justify your choice for each case in a few words."
  <> itemize (ej14_1 <> ej14_2 <> ej14_3)
    where
      ej14_1 = item Nothing <> verb "fructify :: AT a -> AT a" <> ", which grows the tree by replacing each leaf " <> fbox "L" <> " with a branch " <> fbox "B" <> " containing two copies of the leaf."
      ej14_2 = item Nothing <> verb "prune :: a -> (a -> Bool) -> AT a -> AT a" <> ", with " <> fbox "prune z p t" <> " replacing a branch of " <> fbox "t" <> " with a leaf carrying the default value " <> fbox "z" <> " whenever any of the leaves directly on it satisfies the test " <> fbox "p" <> "."
      ej14_3 = item Nothing <> verb "reproduce :: (a -> b) -> (a -> b) -> AT a -> AT b" <> ", with " <> newline <> fbox "reproduce f g t" <> " resulting in a new tree with two modified copies of " <> fbox "t" <> " on the root branch. The left copy is obtained by applying " <> fbox "f" <> " to the values in " <> fbox "t" <> ", and the same goes for " <> fbox "g" <> " and the right copy."


ejercicio15 :: LaTeXC l => l
ejercicio15 =
  item Nothing <> "There is another legal instance of " <> fbox "Applicative" <> " for " <> fbox "AT" <> " (the reversed sequencing version of the original one doesn't count). Write it. "
  <> par <> "Hint: this other instance can be used to implement"
  <> verbatim "  sagittalMap :: (a -> b) -> (a -> b) -> AT a -> AT b  \n"
  <> "which, when given a branch, maps one function over the left child tree and the other over the right child tree."


ejercicio16 :: LaTeXC l => l
ejercicio16 =
  item Nothing <> "Write implementations for " <> fbox "unit" <> " and " <> formatSymbol "(*&*)" <> " in terms of " <> fbox "pure" <> " and " <> formatSymbol "(<*>)" <> ", and vice-versa."


ejercicio17 :: LaTeXC l => l
ejercicio17 = 
  item Nothing <> "Formulate the law of commutative applicative functors," 
  <> format "   liftA2 f u v = liftA2 (flip f) v u -- Commutativity  \n      -- Or, equivalently,  \n   f <$> u <*> v = flip f <$> v <*> u  \n"
  <> " in terms of the " <> fbox "Monoidal" <> " methods."


ejercicio18 :: LaTeXC l => l
ejercicio18 = item Nothing <> "Write from scratch " <> fbox "Monoidal" <> " instances for:" <> itemize (item Nothing <> fbox "ZipList" <> item Nothing <> funcType)


ejercicio19 :: LaTeXC l => l
ejercicio19 = item Nothing <> "Implement a function " <> verb "rollNDiceIO :: Int -> IO [Int]" <> " that, given an integer (a number of die rolls), returns a list of that number of pseudo-random integers between 1 and 6."


ejercicio20  :: LaTeXC l => l
ejercicio20 = item Nothing <> "Implement a function " <> verb "rollDice :: StdGen -> ((Int, Int), StdGen)" <> " that, given a generator, returns a tuple with our random numbers as first element and the last generator as the second."


ejercicio21 :: LaTeXC l => l
ejercicio21 = item Nothing <> "Similarly to what was done for " <> fbox "rollNDiceIO" <> ", implement a function " <> format "  rollNDice :: Int -> State StdGen [Int]  \n" <> " that, given an integer, returns a list with that number of pseudo-random integers between 1 and 6."


ejercicio22 :: LaTeXC l => l
ejercicio22 = 
  item Nothing <> "Write an instance of " <> fbox "Functor" <> " for " <> fbox "State s" <> ". Your final answer should not use anything that mentions " <> fbox "Monad" <> " in its type (that is, " <> fbox "return" <> ", " <> bind <> ", etc.). Then, explain in a few words what the " <> fbox "fmap" <> " you wrote does."
  <> par <> "(Hint: If you get stuck, have another look at the comments about " <> fbox "liftM" <> " in the main body.)"


ejercicio23 :: LaTeXC l => l
ejercicio23 =
  item Nothing <> "Besides " <> fbox "put" <> " and " <> fbox "get" <> ", there are also "
  <> format "  modify :: (s -> s) -> State s ()  \n"
  <> "which modifies the current state using a function, and"
  <> format "  gets :: (s -> a) -> State s a  \n"
  <> "which produces a modified copy of the state while leaving the state itself unchanged. Write implementations for them."


ejercicio24 :: LaTeXC l => l
ejercicio24 = item Nothing <> "If you are not convinced that " <> fbox "State" <> " is worth using, try to implement a function equivalent to " <> fbox "evalState allTypes" <> " without making use of monads, i.e. with an approach similar to " <> fbox "clumsyRollDice" <> " above."


ejercicio25 :: LaTeXC l => l
ejercicio25 = item Nothing <> "Prove the MonadPlus laws for Maybe and the list monad."


ejercicio26 :: LaTeXC l => l
ejercicio26 =
  item Nothing <> "We could augment our above parser to involve a parser for any character: "
  <> format t
  <> "It would then be possible to write a " <> fbox "hexChar" <> " function which parses any valid hexadecimal character (0-9 or a-f). Try writing this function "
  <> par <> "(hint: " <> verb "map digit [0..9] :: [String -> Maybe Int]" <> ")."
  where
    t =
      "  -- | Consume a given character in the input, and return the character we            \n" 
      <> "  --   just consumed, paired with rest of the string. We use a do-block  so that \n"
      <> "  --   if the pattern match fails at any point, fail of the Maybe monad (i.e.        \n"
      <> "  --   Nothing) is returned.                                                       \n"
      <> "  char :: Char -> String -> Maybe (Char, String)                                   \n"
      <> "  char c s = do                                                          \n"
      <> "    let (c':s') = s                                                         \n"
      <> "    if c == c' then Just (c, s') else Nothing                         \n"


ejercicio27 :: LaTeXC l => l
ejercicio27 = item Nothing <> "Why is it that the " <> fbox "lift" <> " function has to be defined separately for each monad, where as " <> fbox "liftM" <> " can be defined in a universal way?"


ejercicio28 :: LaTeXC l => l
ejercicio28 = 
  item Nothing <> fbox "Identity" <> " is a trivial functor, defined in " <> fbox "Data.Functor.Identity" <> " as:"
  <> format "  newtype Identity a = Identity { runIdentity :: a }"
  <> "It has the following Monad instance: "
  <> format "  instance Monad Identity where  \n      return a = Identity a  \n      m >>= k  = k (runIdentity m)  \n"
  <> "Implement a monad transformer " <> fbox "IdentityT" <> ", analogous to " <> fbox "Identity" <> " but wrapping values of type " <> fbox "m a" <> " rather than " <> fbox "a" <> ". Write at least its " <> fbox "Monad" <> " and " <> fbox "MonadTrans" <> " instances."


ejercicio29 :: LaTeXC l => l
ejercicio29 =
  item Nothing <> "Implement " <> verb "state :: MonadState s m => (s -> (a, s)) -> m a" <> " in terms of " <> fbox "get" <> " and " <> fbox "put" <> "."


ejercicio30 :: LaTeXC l => l
ejercicio30 = item Nothing <> "Are " <> fbox "MaybeT (State s)" <> " and " <> fbox "StateT s Maybe" <> " equivalent? (Hint: one approach is comparing what the " <> fbox "run...T" <> " unwrappers produce in each case.)"


ejercicio31 :: LaTeXC l => l
ejercicio31 = 
  item Nothing <> "As was mentioned, any partial order " <> math ("(P,"<=:" " ) <> " is a category with objects the elements of P and a morphism between elements " 
  <> math "a" <> " and " <> math "b" <> " iff " <> math ( "a" <=: "b" ) <> ". Which of the above laws guarantees the transitivity of " <> math (" "<=:" ") <> " ? "


ejercicio32 :: LaTeXC l => l
ejercicio32 = item Nothing <> "Check the functor laws for the Maybe and list functors."


ejercicio33 :: LaTeXC l => l
ejercicio33 = 
  item Nothing <> "Verify that the list and " <> fbox "Maybe" <> " monads do in fact obey the first monad law, "
  <> format "  join . fmap join = join . join  \n"
  <> " with some examples to see precisely how the layer flattening works."


ejercicio34 :: LaTeXC l => l
ejercicio34 = 
  item Nothing <> "Prove the second monad law, " <> fbox "join . fmap return = join . return = id" <> " for the " <> fbox "Maybe" <> " monad."


ejercicio36 :: LaTeXC l => l
ejercicio36 =
  item Nothing <> "In fact, the two versions of the laws we gave: "
  <> format t
  <> "are entirely equivalent. We showed that we can recover the functional laws from the categorical ones. Go the other way; show that starting from the functional laws, the categorical laws hold. It may be useful to remember the following definitions:"
  <> format "  join m = m >>= id  \n  fmap f m = m >>= return . f  \n"
  where 
    t =
      "  -- Categorical:                              \n"
      <> "  join . fmap join = join . join            \n"
      <> "  join . fmap return = join . return = id   \n"
      <> "  return . f = fmap f . return              \n"
      <> "  join . fmap (fmap f) = fmap f . join   \n \n"
      <> "  -- Functional:                            \n"
      <> "  m >>= return = m                          \n"
      <> "  return m >>= f = f m                      \n"
      <> "  (m >>= f) >>= g = m >>= (\\x -> f x >>= g) \n"


ejercicio35 :: LaTeXC l => l
ejercicio35 = item Nothing <> "Convince yourself that the 3rd and 4th laws should hold true for any monad by exploring what they mean, in a similar style to how the first and second laws were explored."




