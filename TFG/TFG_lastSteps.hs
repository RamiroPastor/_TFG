{-# LANGUAGE OverloadedStrings #-}


module TFG_lastSteps where



import Data.Text (pack)


import Text.LaTeX hiding (bot)
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath hiding (bot)
import Text.LaTeX.Packages.Graphicx

import TFG_categorias
import TFG_monadasHaskell (appClass)
import TFG_tablaMonadas (tablaMonadVsApp, tablaEqLaws, table_Sets, table_Lists)
import TFG_comandoChangeMargin



-------- SIMBOLOS U OTRO CODIGO --------

format :: LaTeXC l => String -> l
format = verbatim . pack

formatSym :: LaTeXC l => l -> l
formatSym = scriptsize . fbox . math

c :: Char
c = toEnum 34

bot :: LaTeXC l => l
bot = math ( raw "\\bot" )

footNt1 :: LaTeXC l => l
footNt1 =
  "Experienced category theorists will notice that we're simplifying things a bit here; instead of presenting " <> textit "unit" <> " and " <> textit "join"
  <> " as natural transformations, we treat them explicitly as morphisms, and require naturality as extra axioms alongside the standard monad laws (laws 3 and 4). The reasoning is simplicity;"
  <> " we are not trying to teach category theory as a whole, simply give a categorical background to some of the structures in Haskell. You may also notice that we are giving these morphisms names suggestive of their Haskell analogues, because the names "
  <> math eta <> " and " <> math mu <> " don't provide much intuition."


----------------------------------------


cap3 :: LaTeXC l => l
cap3 =
  chapter "Last Steps"
  <> cap3introduction
  <> section ("Revisiting the " <> textit "Applicative" <> " class") <> appExtended
  <> newpage
  <> section "Still for the curious: The Hask Category" <> haskCat


cap3introduction :: LaTeXC l => l
cap3introduction =
  "In this chapter we revisit everything we have seen in previous chapters, "
  <> "linking the Haskell concepts to what we saw on chapter two on category theory."


appExtended :: LaTeXC l => l
appExtended =
  "A more-in-depth look at the " <> fbox "Applicative" <> " class. The first subsection is just the same text as in chapter 2."
  <> subsection ( textit "Applicative" <> " recap" )
  <> appClass
  <> subsection "Deja vu" <> dejaVu
  <> (subsection $ textit "ZipList") <> zipListSection
  <> subsection "Sequencing of effects" <> seqEffects
  <> subsection "A sliding scale of power" <> powerScale
  <> subsection "The monoidal presentation" <> monoidalPresentation
  <> subsection "Class heritage" <> format "\n \n" <> ( margenesEstrechos $ center $ includegraphics [IGScale 0.6] "Typeclassopedia-diagram" )



dejaVu :: LaTeXC l => l
dejaVu =
  "Does " <> fbox "pure" <> " remind you of anything?"
  <> format "  pure :: Applicative f => a -> f a  \n"
  <> "The only difference between that and..."
  <> format "  return :: Monad m => a -> m a   \n"
  <> " ... is the class constraint. " <> fbox "pure" <> " and " <> fbox "return" <> " serve the same purpose; that is, bringing values into functors. The uncanny resemblances do not stop here. In the appendix about " <> fbox "State" <> " we mention a function called " <> fbox "ap" <> "..."
  <> format "  ap :: (Monad m) => m (a -> b) -> m a -> m b   \n"
  <> " ... which could be used to make functions with many arguments less painful to handle in monadic code:"
  <> allTypes
  <> fbox "ap" <> " looks a lot like " <> formatSym "(<*>)" <> "."
  <> par <> "Those, of course, are not coincidences. " <> fbox "Monad" <> " inherits from " <> fbox "Applicative" <> "... "
  <> format "  Prelude> :info Monad   \n  class Applicative m => Monad (m :: * -> *) where  \n  --etc.   \n"
  <> " ... because " <> fbox "return" <> " and " <> formatSym "(>>=)" <> " are enough to implement " <> fbox "pure" <> " and " <> formatSym "(<*>)"
  <> footnote ( "And if the " <> fbox "Monad" <> " instance follows the monad laws, the resulting " <> fbox "pure" <> " and " <> formatSym "(<*>)" <> " will automatically follow the applicative laws." )
  <> pureANDap
  <> "Several other monadic functions have more general applicative versions. Here are a few of them:"
  <> format "\n "
  <> center tablaMonadVsApp
  <> format "\n "
    where
      allTypes = format aux1
      pureANDap = format aux2
      aux2 = "  pure = return   \n  (<*>) = ap   \n \n  ap u v = do   \n      f <- u   \n      x <- v   \n      return (f x)   \n"
      aux1 =
        "  allTypes :: GeneratorState (Int, Float, Char, Integer, Double, Bool, Int)   \n"
        <> "  allTypes = liftM (,,,,,,) getRandom   \n"
        <> "                       `ap` getRandom   \n"
        <> "                       `ap` getRandom   \n"
        <> "                       `ap` getRandom   \n"
        <> "                       `ap` getRandom   \n"
        <> "                       `ap` getRandom   \n"
        <> "                       `ap` getRandom   \n"




zipListSection  :: LaTeXC l => l
zipListSection =
  "Lists are applicative functors as well. Specialised to lists, the type of " <> formatSym "(<*>)" <> " becomes... "
  <> format "  [a -> b] -> [a] -> [b]  \n"
  <> " ... and so " <> formatSym "(<*>)" <> " applies a list of functions to another list. But exactly how is that done? "
  <> par <> "The standard instance of " <> fbox "Applicative" <> " for lists, which follows from the " <> fbox "Monad" <> " instance, applies every function to every element, like an explosive version of " <> fbox "map" <> "."
  <> format "  Prelude> [(2*),(5*),(9*)] <*> [1,4,7]   \n  [2,8,14,5,20,35,9,36,63]   \n"
  <> par <> "Interestingly, there is another reasonable way of applying a list of functions. Instead of using every combination of functions and values, we can match each function with the value in the corresponding position in the other list. A Prelude function which can be used for that is " <> fbox "zipWith" <> ":"
  <> format "  Prelude> :t zipWith   \n  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]   \n  Prelude> zipWith ($) [(2*),(5*),(9*)] [1,4,7]   \n  [2,20,63]   \n"
  <> "When there are two useful possible instances for a single type, the dilemma is averted by creating a " <> fbox "newtype" <> " which implements one of them. In this case, we have " <> fbox "ZipList" <> ", which lives in " <> fbox "Control.Applicative" <> ":"
  <> format "  newtype ZipList a = ZipList { getZipList :: [a] }   \n"
  <> par <> "We have already seen what " <> formatSym "(<*>)" <> " should be for zip-lists; all that is needed is to add the " <> fbox "newtype" <> " wrappers:"
  <> format "  instance Applicative ZipList where   \n      (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)   \n      pure x                        = undefined -- TODO   \n"
  <> par <> "As for " <> fbox "pure" <> ", it is tempting to use " <> fbox "pure x = ZipList [x]" <> ", following the standard list instance. We can't do that, however, as it violates the applicative laws. According to the identity law:"
  <> format "  pure id <*> v = v   \n"
  <> par <> "Substituting " <> formatSym "(<*>)" <> " and the suggested " <> fbox "pure" <> ", we get:"
  <> format "  ZipList [id] <*> ZipList xs = ZipList xs   \n  ZipList (zipWith ($) [id] xs) = ZipList xs   \n"
  <> par <> "Now, suppose " <> fbox "xs" <> " is the infinite list " <> fbox "[1..]" <> ":"
  <> format "  ZipList (zipWith ($) [id] [1..]) = ZipList [1..]   \n  ZipList [1] = ZipList [1..]   \n  [1] = [1..] -- Obviously false!   \n"
  <> par <> "The problem is that " <> fbox "zipWith" <> " produces lists whose length is that of the shortest list passed as argument, and so " <> verb "(ZipList [id] <*>)" <> " will cut off all elements of the other zip-list after the first. The only way to ensure " <> verb "zipWith ($) fs" <> " never removes elements is making " <> fbox "fs" <> " infinite. The correct " <> fbox "pure" <> " follows from that:"
  <> format "  instance Applicative ZipList where   \n      (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)   \n      pure x                        = ZipList (repeat x)   \n"
  <> par <> "The " <> fbox "ZipList" <> " applicative instance offers an alternative to all the zipN and zipWithN functions in " <> fbox "Data.List" <> " which can be extended to any number of arguments:"
  <> format "  >>> import Control.Applicative   \n  >>> ZipList [(2*),(5*),(9*)] <*> ZipList [1,4,7]   \n  ZipList {getZipList = [2,20,63]}   \n  >>> (,,) <$> ZipList [1,4,9] <*> ZipList [2,8,1] <*> ZipList [0,0,9]   \n  ZipList {getZipList = [(1,2,0),(4,8,0),(9,1,9)]}   \n  >>> liftA3 (,,) (ZipList [1,4,9]) (ZipList [2,8,1]) (ZipList [0,0,9])   \n  ZipList {getZipList = [(1,2,0),(4,8,0),(9,1,9)]}   \n"




seqEffects :: LaTeXC l => l
seqEffects =
  "As we have just seen, the standard " <> fbox "Applicative" <> " instance for lists applies every function in one list to every element of the other. That, however, does not specify " <> formatSym "(<*>)" <> " unambiguously. To see why, try to guess what is the result of "
  <> format "  [(2*),(3*)]<*>[4,5]   \n"
  <> " without looking at the example above or the answer just below."
  <> format "  Prelude> [(2*),(3*)] <*> [4,5]  \n \n \n  --- ... \n \n \n [8,10,12,15] \n"
  <> "Unless you were paying very close attention or had already analysed the implementation of " <> formatSym "(<*>)" <> ", the odds of getting it right were about even. The other possibility would be " <> fbox "[8,12,10,15]"
  <> ". The difference is that for the first (and correct) answer the result is obtained by taking the skeleton of the first list and replacing each element by all possible combinations with elements of the second list, while for the other possibility the starting point is the second list."
  <> par <> "In more general terms, the difference between is one of " <> textit "sequencing of effects" <> ". Here, by effects we mean the functorial context, as opposed to the values within the functor (some examples: the skeleton of a list, actions performed in the real world in "
  <> fbox "IO" <> ", the existence of a value in " <> fbox "Maybe" <> "). The existence of two legal implementations of " <> formatSym "(<*>)" <> " for lists which only differ in the sequencing of events indicates that "
  <> fbox "[ ]" <> " is a non-commutative applicative functor. A " <> textit "commutative" <> " applicative functor, by contrast, leaves no margin for ambiguity in that respect. More formally, a commutative applicative functor is one for which the following holds:"
  <> format "  liftA2 f u v = liftA2 (flip f) v u -- Commutativity  \n"
  <> "Or, equivalently, "
  <> format "  f <$> u <*> v = flip f <$> v <*> u   \n"
  <> par <> "By the way, if you hear about " <> textit "commutative monads" <> " in Haskell, the concept involved is the same, only specialised to " <> fbox "Monad" <> "."
  <> par <> "Commutativity (or the lack thereof) affects other functions which are derived from " <> formatSym "(<*>)" <> " as well. " <> formatSym "(*>)" <> " is a clear example:"
  <> format "  (*>) :: Applicative f => f a -> f b -> f b   \n"
  <> par <> formatSym "(*>)" <> " combines effects while preserving only the values of its second argument. For monads, it is equivalent to " <> formatSym "(>>)" <> ". Here is a demonstration of it using " <> fbox "Maybe" <> ", which is commutative:"
  <> format "  Prelude> Just 2 *> Just 3   \n  Just 3   \n  Prelude> Just 3 *> Just 2   \n  Just 2   \n  Prelude> Just 2 *> Nothing   \n  Nothing   \n  Prelude> Nothing *> Just 2   \n  Nothing   \n"
  <> par <> "Swapping the arguments does not affect the effects (that is, the being and nothingness of wrapped values). For " <> fbox "IO" <> ", however, swapping the arguments does reorder the effects:"
  <> format ( "  Prelude> (print "<>[c]<>"foo"<>[c]<>" *> pure 2) *> (print "<>[c]<>"bar"<>[c]<>" *> pure 3)   \n  "<>[c]<>"foo"<>[c]<>"   \n  "<>[c]<>"bar"<>[c]<>"   \n  3   \n  Prelude> (print "<>[c]<>"bar"<>[c]<>" *> pure 3) *> (print "<>[c]<>"foo"<>[c]<>" *> pure 2)   \n  "<>[c]<>"bar"<>[c]<>"   \n  "<>[c]<>"foo"<>[c]<>"   \n  2   \n" )
  <> par <> "The convention in Haskell is to always implement " <> formatSym "(<*>)" <> " and other applicative operators using left-to-right sequencing. Even though this convention helps reducing confusion, it also means appearances sometimes are misleading. For instance, the "
  <> formatSym "(<*)" <> " function is " <> textit "not " <> verb "flip (*>)" <> ", as it sequences effects from left to right just like " <> formatSym "(*>)" <> ":"
  <> format ( "  Prelude> (print "<>[c]<>"foo"<>[c]<>" *> pure 2) <* (print "<>[c]<>"bar"<>[c]<>" *> pure 3)   \n  "<>[c]<>"foo"<>[c]<>"   \n  "<>[c]<>"bar"<>[c]<>"   \n  2   \n" )
  <> "For the same reason, " <> verb "(<**>) :: Applicative f => f a -> f (a -> b) -> f b" <> " from " <> fbox "Control.Applicative" <> " is not " <> verb "flip (<*>)"
  <> ". That means it provides a way of inverting the sequencing:"
  <> format "  >>> [(2*),(3*)] <*> [4,5]   \n  [8,10,12,15]   \n  >>> [4,5] <**> [(2*),(3*)]   \n  [8,12,10,15]   \n"
  <> "An alternative is the " <> fbox "Control.Applicative.Backwards" <> " module from " <> fbox "transformers" <> ", which offers a " <> fbox "newtype" <> " for flipping the order of effects:"
  <> format "  newtype Backwards f a = Backwards { forwards :: f a }  \n \n \n"
  <> format "  >>> Backwards [(2*),(3*)] <*> Backwards [4,5]   \n  Backwards [8,12,10,15]   \n"




powerScale  :: LaTeXC l => l
powerScale =
  fbox "Functor" <> ", " <> fbox "Applicative" <> ", " <> fbox "Monad" <> ". Three closely related functor type classes; three of the most important classes in Haskell. Though we have seen many examples of " <> fbox "Functor" <> " and " <> fbox "Monad" <> " in use, and a few of " <> fbox "Applicative"
  <> ", we have not compared them head to head yet. If we ignore " <> fbox "pure" <> "/" <> fbox "return" <> " for a moment, the characteristic methods of the three classes are:"
  <> format "  fmap :: Functor f => (a -> b) -> f a -> f b   \n  (<*>) :: Applicative f => f (a -> b) -> f a -> f b   \n  (>>=) :: Monad m => m a -> (a -> m b) -> m b   \n"
  <> "While those look like disparate types, we can change the picture with a few aesthetic adjustments. Let's replace " <> fbox "fmap" <> " by its infix synonym, " <> formatSym "(<$>)" <> "; " <> formatSym "(>>=)" <> " by its flipped version, " <> formatSym "(=<<)" <> "; and tidy up the signatures a bit:"
  <> format "  (<$>) :: Functor t     =>   (a -> b) -> (t a -> t b)   \n  (<*>) :: Applicative t => t (a -> b) -> (t a -> t b)   \n  (=<<) :: Monad t       => (a -> t b) -> (t a -> t b)   \n"
  <> "Suddenly, the similarities are striking. " <> fbox "fmap" <> ", " <> formatSym "(<*>)" <> " and " <> formatSym "(=<<)" <> " are all mapping functions over " <> fbox "Functor" <> "s."
  <> footnote " It is not just a question of type signatures resembling each other: the similarity has theoretical ballast. One aspect of the connection is that it is no coincidence that all three type classes have identity and composition laws."
  <> " The differences between them are in what is being mapped over in each case: "
  <> itemize ( item Nothing <> fbox "fmap" <> " maps arbitrary functions over functors." <> item Nothing <> formatSym "(<*>)" <> " maps " <> verb "t (a -> b)" <> " morphisms over (applicative) functors." <> item Nothing <> formatSym "(=<<)" <> " maps " <> verb "a -> t b" <> " functions over (monadic) functors." )
  <> par <> "The day-to-day differences in uses of " <> fbox "Functor" <> ", " <> fbox "Applicative" <> " and " <> fbox "Monad" <> " follow from what the types of those three mapping functions allow you to do. As you move from "
  <> fbox "fmap" <> " to " <> formatSym "(<*>)" <> " and then to " <> formatSym "(>>=)" <> ", you gain in power, versatility and control, at the cost of guarantees about the results. We will now slide along this scale. While doing so, we will use the contrasting terms " <> textit "values" <> " and " <> textit "context" <> " to refer to plain values within a functor and to the whatever surrounds them, respectively."
  <> par <> "The type of fmap ensures that it is impossible to use it to change the context, no matter which function it is given. In " <> format "  (a -> b) -> t a -> t b  \n " <> ", the " <> verb "(a -> b)" <> " function has nothing to do with the " <> fbox "t" <> " context of the " <> fbox "t a" <> " functorial value, and so applying it cannot affect the context. For that reason, if you do " <> fbox "fmap f xs" <> " on some list " <> fbox "xs" <> " the number of elements of the list will never change."
  <> format "  Prelude> fmap (2*) [2,5,6]   \n  [4,10,12]   \n"
  <> "That can be taken as a safety guarantee or as an unfortunate restriction, depending on what you intend. In any case, " <> formatSym "(<*>)" <> " is clearly able to change the context:"
  <> format "  Prelude> [(2*),(3*)] <*> [2,5,6]   \n  [4,10,12,6,15,18]   \n"
  <> par <> "The " <> verb "t (a -> b)" <> " morphism carries a context of its own, which is combined with that of the " <> fbox "t a" <> " functorial value. " <> formatSym "(<*>)" <> ", however, is subject to a more subtle restriction. While " <> verb "t (a -> b)"
  <> " morphisms carry context, within them there are plain " <> verb "(a -> b)" <> " functions, which are still unable to modify the context. That means the changes to the context " <> formatSym "(<*>)" <> " performs are fully determined by the context of its arguments, and the values have no influence over the resulting context."
  <> format ( "  Prelude> (print "<>[c]<>"foo"<>[c]<>" *> pure (2*)) <*> (print "<>[c]<>"bar"<>[c]<>" *> pure 3)   \n  "<>[c]<>"foo"<>[c]<>"   \n  "<>[c]<>"bar"<>[c]<>"   \n  6   \n  Prelude> (print "<>[c]<>"foo"<>[c]<>" *> pure 2) *> (print "<>[c]<>"bar"<>[c]<>" *> pure 3)   \n  "<>[c]<>"foo"<>[c]<>"   \n  "<>[c]<>"bar"<>[c]<>"   \n  3   \n  Prelude> (print "<>[c]<>"foo"<>[c]<>" *> pure undefined) *> (print "<>[c]<>"bar"<>[c]<>" *> pure 3)   \n  "<>[c]<>"foo"<>[c]<>"   \n  "<>[c]<>"bar"<>[c]<>"   \n  3   \n" )
  <> "Thus with list " <> formatSym "(<*>)" <> " you know that the length of the resulting list will be the product of the lengths of the original lists, with " <> fbox "IO" <> formatSym "(<*>)" <> " you know that all real world effect will happen as long as the evaluation terminates, and so forth."
  <> par <> "With " <> fbox "Monad" <> ", however, we are in a very different game. " <> formatSym "(>>=)" <> " takes an " <> verb "a -> t b" <> " function, and so it is able to create context from values. That means a lot of flexibility: "
  <> format aux
  <> "Taking advantage of the extra flexibility, however, might mean having less guarantees about, for instance, whether your functions are able to unexpectedly erase parts of a data structure for pathological inputs, or whether the control flow in your application remains intelligible. In some situations there might be performance implications as well, as the complex data dependencies monadic code makes possible might prevent useful refactorings and optimisations."
  <> par <> "All in all, it is a good idea to only use as much power as needed for the task at hand. If you do need the extra capabilities of Monad, go right ahead; however, it is often worth it to check whether " <> fbox "Applicative" <> " or " <> fbox "Functor" <> " are sufficient."
    where
    aux =
      "  Prelude> [1,2,5] >>= \\x -> replicate x x                                     \n"
      <> "  [1,2,2,5,5,5,5,5]                                                          \n"
      <> "  Prelude> [0,0,0] >>= \\x -> replicate x x                                    \n"
      <> "  []                                                                             \n"
      <> "  Prelude> return 3 >>= \\x -> print $ if x < 10 then `Too small' else `OK'   \n"
      <> "  `Too small'                                                                      \n"
      <> "  Prelude> return 42 >>= \\x -> print $ if x < 10 then `Too small' else `OK'   \n"
      <> "  `OK'                                                                          \n"




monoidalPresentation :: LaTeXC l => l
monoidalPresentation =
  "Back in last chapter, we saw how the " <> fbox "Monad" <> " class can be specified using either " <> formatSym "(>=>)" <> " or join instead of " <> formatSym "(>>=)"
  <> ". In a similar way, " <> fbox "Applicative" <> " also has an alternative presentation, which might be implemented through the following type class:"
  <> format "  class Functor f => Monoidal f where   \n      unit  :: f ()   \n      (*&*) :: f a -> f b -> f (a,b)   \n"
  <> "There are deep theoretical reasons behind the name " <> qts "monoidal" <> "."
  <> footnote ( "For extra details, follow the leads from the corresponding section of the Typeclasseopedia (https://wiki.haskell.org/Typeclassopedia#Alternative_formulation) and the blog post by Edward Z. Yang which inspired it. (http://blog.ezyang.com/2012/08/applicative-functors/)" )
  <> " In any case, we can informally say that it does look a lot like a monoid: " <> fbox "unit" <> " provides a default functorial value whose context wraps nothing of interest, and " <> formatSym "(*&*)" <> " combines functorial values by pairing values and combining effects. The "
  <> fbox "Monoidal" <> " formulation provides a clearer view of how " <> fbox "Applicative" <> " manipulates functorial contexts. Naturally, " <> fbox "unit" <> " and " <> formatSym "(*&*)" <> " can be used to define " <> fbox "pure" <> " and " <> formatSym "(<*>)" <> ", and vice-versa."
  <> par <> "The Applicative laws are equivalent to the following set of laws, stated in terms of " <> fbox "Monoidal" <> ":"
  <> format aux
  <> "The functions to the left of the " <> fbox "($)" <> " are just boilerplate to convert between equivalent types, such as " <> fbox "b" <> " and " <> fbox "((), b)"
  <> ". If you ignore them, the laws are a lot less opaque than in the usual " <> fbox "Applicative" <> " formulation. By the way, just like for " <> fbox "Applicative" <> " there is a bonus law, which is guaranteed to hold in Haskell:"
  <> format "  fmap (g *** h) (u *&* v) = fmap g u *&* fmap h v -- Naturality   \n  -- g *** h = \\(x, y) -> (g x, h y)   \n"
    where
    aux =
      "  fmap snd $ unit *&* v = v                    -- Left identity     \n"
      <> "  fmap fst $ u *&* unit = u                    -- Right identity \n"
      <> "  fmap asl $ u *&* (v *&* w) = (u *&* v) *&* w -- Associativity  \n"
      <> "  -- asl (x, (y, z)) = ((x, y), z)                               \n"




haskCat :: LaTeXC l => l
haskCat =
  "In this section, we will dive in the " <> textbf "Hask" <> " category, identifying some Haskell funcstions with their mathematical equivalent in Category Theory."
  <> " Recalling from the first chapter:"
  <> itemize ( item (Just hask) <> " : " <> objCat hask <> " -- the class of all Haskell types. " <> "Hom(" <> hask <> ")" <> " -- Haskell functions." <> " The composition law is the " <> math (mathtt "(.)") <> " operator." )
--  -------------------------------------------------------------------------
  <> subsection ("Checking that " <> textbf "Hask" <> " is a category")
  <> "We can check the first and second law easily: we know " <> fbox "(.)" <> " is an associative function, and clearly, for any " <> fbox "f" <> " and " <> fbox "g" <> ", " <> fbox "f . g" <> " is another function. "
  <> par <> "In Hask, the identity morphism is " <> fbox "id" <> ", and we have trivially:"
  <> format "  id . f = f . id = f  \n"
  <> par <> "This isn't an exact translation of the law above, though; we're missing subscripts. The function " <> fbox "id" <> " in Haskell is " <>textit "polymorphic" <> "-- it can take many different types for its domain and range, or, in category-speak, can have many different source and target objects. But morphisms in category theory are by definition "
  <> textit "monomorphic" <> "-- each morphism has one specific source object and one specific target object. A polymorphic Haskell function can be made monomorphic by specifying its type (" <> textit "instantiating" <> " with a monomorphic type), so it would be more precise if we said that the identity morphism from " <> textbf "Hask" <> " on a type "
  <> fbox "A" <> " is " <> verb "(id :: A -> A)" <> ". With this in mind, the above law would be rewritten as:"
  <> format "  (id :: B -> B) . f = f . (id :: A -> A) = f  \n"
  <> " However, for simplicity, we will ignore this distinction when the meaning is clear."
  <> par <> "Actually, there is a subtlety here: because " <> fbox "(.)" <> " is a lazy function, if " <> fbox "f" <> " is " <> fbox "undefined" <> ", we have that " <> verb "id . f = \\_ -> " <> bot <> " . Now, while this may seem equivalent to " <> bot <> " for all intents and purposes, you can actually tell them apart using the strictifying function "
  <> fbox "seq" <> ", meaning that the last category law is broken. We can define a new strict composition function, "
  <> format "  f .! g = ((.) $! f) $! g   \n"
  <> " that makes " <> textbf "Hask" <> " a category. We proceed by using the normal " <> fbox "(.)" <> ", though, and attribute any discrepancies to the fact that " <> fbox "seq" <> " breaks an awful lot of the nice language properties anyway."
--  -------------------------------------------------------------------------
  <> subsection ( "Functors on " <> textbf "Hask" )
  <> "The Functor typeclass you have probably seen in Haskell does in fact tie in with the categorical notion of a functor. Remember that a functor has two parts: it maps objects in one category to objects in another and morphisms in the first category to morphisms in the second. Functors in Haskell are from "
  <> textbf "Hask" <> " to " <> textit "func" <> ", where " <> textit "func" <> " is the subcategory of " <> textbf "Hask" <> " defined on just that functor's types. E.g. the list functor goes from "
  <> textbf "Hask" <> " to " <> textbf "Lst" <> ", where " <> textbf "Lst" <> " is the category containing only " <> textit "list types" <> ", that is, " <> fbox "[T]" <> " for any type " <> fbox "T" <> ". The morphisms in " <> textbf "Lst" <> " are functions defined on list types, that is, functions "
  <> verb "[T] -> [U]" <> " for types " <> fbox "T" <> ", " <> fbox "U" <> ". How does this tie into the Haskell typeclass " <> fbox "Functor" <> "? Recall its definition:"
  <> format "  class Functor (f :: * -> *) where  \n    fmap :: (a -> b) -> f a -> f b  \n"
  <> par <> "Let's have a sample instance, too:"
  <> format "  instance Functor Maybe where  \n    fmap f (Just x) = Just (f x)  \n    fmap _ Nothing  = Nothing  \n"
  <> par <> "Here's the key part: the " <> textit "type constructor " <> fbox "Maybe" <> " takes any type " <> fbox "T" <> " to a new type, " <> fbox "Maybe T" <> ". Also, " <> fbox "fmap" <> " restricted to Maybe types takes a function " <> verb "a -> b" <> " to a function " <> verb "Maybe a -> Maybe b"
  <> ". But that's it! We've defined two parts, something that takes objects in " <> textbf "Hask" <> " to objects in another category (that of Maybe types and functions defined on Maybe types), and something that takes morphisms in "
  <> textbf "Hask" <> " to morphisms in this category. So Maybe is a functor."
  <> par <> "A useful intuition regarding Haskell functors is that they represent types that can be mapped over. This could be a list or a Maybe, but also more complicated structures like trees. A function that does some mapping could be written using " <> fbox "fmap" <> ", then any functor structure could be passed into this function. "
  <> "E.g. you could write a generic function that covers all of " <> fbox "Data.List.map" <> ", " <> fbox "Data.Map.map" <> ", " <> fbox "Data.Array.IArray.amap" <> ", and so on."
  <> par <> "What about the functor axioms? The polymorphic function " <> fbox "id" <> " takes the place of " <> math (raw "id_{A}" ) <> " for any " <> fbox "A" <> ", so the first law states:"
  <> format "  fmap id = id   \n"
  <> par <> "With our above intuition in mind, this states that mapping over a structure doing nothing to each element is equivalent to doing nothing overall. Secondly, morphism composition is just " <> fbox "(.)" <> ", so "
  <> format "  fmap (f . g) = fmap f . fmap g   \n"
  <> "This second law is very useful in practice. Picturing the functor as a list or similar container, the right-hand side is a two-pass algorithm: we map over the structure, performing " <> fbox "g" <> ", then map over it again, performing " <> fbox "f"
  <> ". The functor axioms guarantee we can transform this into a single-pass algorithm that performs " <> fbox "f . g" <> ". This is a process known as " <> textit "fusion" <> "."
--  -------------------------------------------------------------------------
  <> subsubsection "Translating categorical concepts into Haskell"
  <> "Functors provide a good example of how category theory gets translated into Haskell. The key points to remember are that:"
  <> itemize ( item Nothing <> "We work in the category " <> textbf "Hask" <> " and its subcategories." <> item Nothing <> "Objects are types. " <> item Nothing <> "Morphisms are functions. " <> "Things that take a type and return another type are type constructors. " <> item Nothing <> "Things that take a function and return another function are higher-order functions." <> item Nothing <> "Typeclasses, along with the polymorphism they provide, make a nice way of capturing the fact that in category theory things are often defined over a number of objects at once." )
--  -------------------------------------------------------------------------
  <> subsection "Monads"
  <> "Monads are obviously an extremely important concept in Haskell, and in fact they originally came from category theory."
  <> footnote footNt1
  <> " A monad is a special type of functor, from a category to that same category, that supports some additional structure. So, down to definitions. A monad is a functor " <> math ( "M:C"<>to<>" C" ) <> ", along with two morphisms for every object X in C:"
  <> itemize ( item Nothing <> math ( ("unit"!:"X"^:"M") <> ": X" <> to <> "M(X)" ) <> item Nothing <> math ( "join"!:"X"^:"M" <> "M(M(X))" <> to <> "M(X)" )  )
  <> "When the monad under discussion is obvious, we'll leave out the " <> math (" "^:"M") <> " superscript for these functions and just talk about " <> math ("unit"!:"X") <> " and " <> math ("join"!:"X") <> " for some " <> math "X" <> "."
  <> subsubsection "Translating"
  <> "Let's see how this translates to the Haskell typeclass Monad, then."
  <> format "  class Functor m => Monad m where   \n    return :: a -> m a   \n    (>>=)  :: m a -> (a -> m b) -> m b   \n"
  <> par <> "The class constraint of " <> fbox "Functor m" <> " ensures that we already have the functor structure: a mapping of objects and of morphisms. " <> fbox "return" <> " is the (polymorphic) analogue to "
  <> math ("unit"!:"X") <> " for any " <> math "X" <> ". But we have a problem. "
  <> par <> "Although " <> fbox "return" <> "'s type looks quite similar to that of " <> textit "unit" <> "; the other function, " <> formatSym "(>>=)" <> ", often called " <> textit "bind" <> ", bears no resemblance to " <> textit "join" <> "."
  <> " There is however another monad function, "
  <> format "  join :: Monad m => m (m a) -> m a  \n"
  <> " that looks quite similar. Indeed, we can recover " <> fbox "join" <> " and " <> formatSym "(>>=)" <> " from each other:"
  <> format "  join :: Monad m => m (m a) -> m a  \n  join x = x >>= id  \n \n  (>>=) :: Monad m => m a -> (a -> m b) -> m b  \n  x >>= f = join (fmap f x)  \n"
  <> "So specifying a monad's " <> fbox "return" <> ", " <> fbox "fmap" <> ", " <> " and " <> fbox "join" <> " is equivalent to specifying its " <> fbox "return" <> fbox "fmap" <> ", " <> " and " <> formatSym "(>>=)"
  <> ". It just turns out that the normal way of defining a monad in category theory is to give " <> fbox "unit" <> " and " <> fbox "join" <> ", whereas Haskell programmers like to give " <> fbox "return" <> " and " <> fbox "bind."
  <> footnote ( "This is perhaps due to the fact that Haskell programmers like to think of monads as a way of sequencing computations with a common feature, whereas in category theory the container aspect of the various structures is emphasised. " <> fbox "join" <> " pertains naturally to containers (squashing two layers of a container down into one), but " <> formatSym "(>>=)" <> " is the natural sequencing operation (do something, feeding its results into something else)." )
  <> " Often, the categorical way makes more sense. Any time you have some kind of structure " <> textit "M" <> " and a natural way of taking any object " <> math "X" <> " into " <> math "M(X)"
  <> ", as well as a way of taking " <> math "M(M(X))" <> " into " <> math "M(X)" <> ", you probably have a monad. We can see this in the following example section."
  <> subsubsection "Example: the powerset functor is also a monad"
  <> "The power set functor " <> math ( "P:"<>(mathbf "Set") <>to<>(mathbf "Set") ) <> " forms a monad. For any set " <> math "S" <> " you have a "
  <> math ("unit"!:"S" <> "(x)" ) <> " " <> math "=" <> " "<> math "{x}" <> ", mapping elements to their singleton set. Note that each of these singleton sets are trivially a subset of " <> math "S"
  <> ", so " <> math ( "unit"!:"S" ) <> " returns elements of the powerset of " <> math "S" <> ", as is required. Also, you can define a function " <> math ( "join"!:"S" ) <> " as follows: we receive an input "
  <> math ( ("L" `in_` (mathcal "P")) <> "(" <> mathcal "P" <> "(S))" )
  <> ". This is:" <> itemize ( item Nothing <> "A member of the powerset of the powerset of S." <> item Nothing <> "So a member of the set of all subsets of the set of all subsets of S." <> item Nothing <> "So a set of subsets of S." )
  <> "We then return the union of these subsets, giving another subset of S. Symbolically, "
  <> equation_ (  (("join"!:"S")<>"(L)") =: (raw "\\bigcup" <> " L")  )
  <> "Hence " <> math (mathcal "P") <> " is a monad."
  <> footnote "If you can prove that certain laws hold, which we'll explore with lists in the next subsection."
  <> par <> "In fact, " <> math (mathcal "P") <> " is almost equivalent to the list monad; with the exception that we're talking lists instead of sets, they're almost the same. Compare:"
  <> format "\n "
  <> par <> "Power set functor on " <> textbf "Set" <> ", given a set " <> math "S" <> " and a morphism " <> math ("f : A "<>to<>"B")
  <> format "\n "
  <> center table_Sets
  <> format "\n \n "
  <> par <> "List monad from Haskell, given a type " <> fbox "T" <> " and a function " <> verb "f :: A -> B"
  <> format "\n "
  <> center table_Lists
  <> format "\n "
--  -------------------------------------------------------------------------
  <> subsection "The monad laws and their importance"
  <> "Just as functors had to obey certain axioms in order to be called functors, monads have a few of their own. We'll first list them, then translate to Haskell, then see why they're important."
  <> par <> "Given a monad " <> math ("M : C"<>to<>"C") <> " and a morphism " <> math ("f:A"<>to<>"B") <> " for " <> math ("A,B"`in_`"C") <> ", "
  <> enumerate ( item Nothing <> mathAux1 <> item Nothing <> mathAux2 <> item Nothing <> mathAux3 <> item Nothing <> mathAux4 )
  <> par <> "By now, the Haskell translations should be hopefully self-explanatory:"
  <> enumerate ( item Nothing <> haskAux1 <> item Nothing <> haskAux2 <> item Nothing <> haskAux3 <> item Nothing <> haskAux4 )
  <> par <> "(Remember that " <> fbox "fmap" <> " is the part of a functor that acts on morphisms.) These laws seem a bit impenetrable at first, though. What on earth do these laws mean, and why should they be true for monads? Let's explore the laws."
  <> subsubsection "The first law" <> fstLaw
  <> subsubsection "The second law" <> sndLaw
  <> subsubsection "The third and fourth laws" <> thirdAND4th
  <> subsubsection "Application to do-blocks" <> moreDOblocks

mathAux1 :: LaTeXC l => l
mathAux1 = math ( ("join" `circ` "M(join)") =: ("join" `circ` "join") )
mathAux2 :: LaTeXC l => l
mathAux2 = math ( ("join" `circ` "M(unit)") =: ("join" `circ` "unit") =: "id"  )
mathAux3 :: LaTeXC l => l
mathAux3 = math ( ("unit" `circ` "f") =: ("M(f)" `circ` "unit")  )
mathAux4 :: LaTeXC l => l
mathAux4 = math ( ("join" `circ` "M(M(f)") =: ("M(f)" `circ` "join")  )
haskAux1 :: LaTeXC l => l
haskAux1 = fbox "join . fmap join = join . join"
haskAux2 :: LaTeXC l => l
haskAux2 = fbox "join . fmap return = join . return = id"
haskAux3 :: LaTeXC l => l
haskAux3 = fbox "return . f = fmap f . return"
haskAux4 :: LaTeXC l => l
haskAux4 = fbox "join . fmap (fmap f) = fmap f . join"



fstLaw :: LaTeXC l => l
fstLaw =
  haskAux1
  <> par <> "In order to understand this law, we'll first use the example of lists. The first law mentions two functions, " <> fbox "join . fmap join" <> " (the left-hand side) and " <> fbox "join . join"
  <> " (the right-hand side). What will the types of these functions be? Remembering that " <> fbox "join" <> "'s type is " <> verb "[[a]] -> [a]" <> " (we're talking just about lists for now), the types are both "
  <> verb "[[[a]]] -> [a]" <> " (the fact that they're the same is handy; after all, we're trying to show they're completely the same function!). So we have a list of lists of lists. The left-hand side, then, performs "
  <> fbox "fmap join" <> " on this 3-layered list, then uses " <> fbox "join" <> " on the result. " <> fbox "fmap" <> " is just the familiar " <> fbox "map"
  <> " for lists, so we first map across each of the list of lists inside the top-level list, concatenating them down into a list each. Now we have a list of lists, which we then run through "
  <> fbox "join" <> ". In summary, we `enter' the top level, collapse the second and third levels down, then collapse this new level with the top level."
  <> par <> "What about the right-hand side? We first run " <> fbox "join" <> " on our list of list of lists. Although this is three layers, and you normally apply a two-layered list to "
  <> fbox "join" <> ", this will still work, because a " <> fbox "[[[a]]]" <> " is just " <> fbox "[[b]]" <> ", where " <> fbox "b = [a]"
  <> ", so in a sense, a three-layered list is just a two layered list, but rather than the last layer being `flat', it is composed of another list. So if we apply our list of lists (of lists) to "
  <> fbox "join" <> ", it will flatten those outer two layers into one. As the second layer wasn't flat but instead contained a third layer, we will still end up with a list of lists, which the other join flattens"
  <> ". Summing up, the left-hand side will flatten the inner two layers into a new layer, then flatten this with the outermost layer. The right-hand side will flatten the outer two layers, then flatten this with the innermost layer. These two operations should be equivalent. It's sort of like a law of associativity for " <> fbox "join" <> "."
  <> par <> fbox "Maybe" <> " is also a monad, with "
  <> format "  return :: a -> Maybe a  \n  return x = Just x  \n \n  join :: Maybe (Maybe a) -> Maybe a  \n  join Nothing         = Nothing  \n  join (Just Nothing)  = Nothing  \n  join (Just (Just x)) = Just x  \n"
  <> "So if we had a " <> textit "three" <> "-layered Maybe (i.e., it could be " <> fbox "Nothing" <> ", "  <> fbox "Just Nothing" <> ", " <> fbox "Just (Just Nothing)" <> " or " <> fbox "Just (Just (Just x)))"
  <> ", the first law says that collapsing the inner two layers first, then that with the outer layer is exactly the same as collapsing the outer layers first, then that with the innermost layer."




sndLaw :: LaTeXC l => l
sndLaw =
  haskAux2
  <> par <> "What about the second law, then? Again, we'll start with the example of lists. Both functions mentioned in the second law are functions "
  <> verb "[a] -> [a]" <> ". The left-hand side expresses a function that maps over the list, turning each element " <> fbox "x" <> " into its singleton list " <> fbox "[x]"
  <> ", so that at the end we're left with a list of singleton lists. This two-layered list is flattened down into a single-layer list again using the " <> fbox "join"
  <> ". The right hand side, however, takes the entire list " <> fbox "[x, y, z, ...]" <> ", turns it into the singleton list of lists " <> fbox "[[x, y, z, ...]]"
  <> ", then flattens the two layers down into one again. This law is less obvious to state quickly, but it essentially says that applying " <> fbox "return" <> " to a monadic value, then "
  <> fbox "join" <> "ing the result should have the same effect whether you perform the " <> fbox "return" <> " from inside the top layer or from outside it."




thirdAND4th :: LaTeXC l => l
thirdAND4th =
  haskAux3 <> "\n \n" <> noindent <> haskAux4
  <> par <> "The last two laws express more self evident fact about how we expect monads to behave. The easiest way to see how they are true is to expand them to use the expanded form:"
  <> aux1 <> aux2
    where
      aux1 = format "1.   \\x -> return (f x) = \\x -> fmap f (return x)   \n"
      aux2 = format "2.   \\x -> join (fmap (fmap f) x) = \\x -> fmap f (join x)   \n"



moreDOblocks :: LaTeXC l => l
moreDOblocks =
  "Well, we have intuitive statements about the laws that a monad must support, but why is that important? The answer becomes obvious when we consider do-blocks. Recall that a do-block is just syntactic sugar for a combination of statements involving " <> formatSym "(>>=)" <> " as witnessed by the usual translation:"
  <> format "  do { x }                 -->  x   \n  do { let { y = v }; x }  -->  let y = v in do { x }   \n  do { v <- y; x }         -->  y >>= \\v -> do { x }   \n  do { y; x }              -->  y >>= \\_ -> do { x }   \n"
  <> par <> "Also notice that we can prove what are normally quoted as the monad laws using " <> fbox "return" <> " and " <> formatSym "(>>=)" <> " from our above laws (the proofs are a little heavy in some cases, feel free to skip them if you want to):"
  <> format "1.  return x >>= f = f x   -- First Law \n"
  <> par <> "Proof:"
  <> format "     return x >>= f   \n   = join (fmap f (return x)) -- By the definition of (>>=)   \n   = join (return (f x))      -- By law 3   \n   = (join . return) (f x)   \n   = id (f x)                 -- By law 2   \n   = f x   \n \n"
  <> format "2.  m >>= return = m   -- Second Law \n"
  <> par <> "Proof:"
  <> format "    m >>= return   \n   = join (fmap return m)    -- By the definition of (>>=)   \n   = (join . fmap return) m   \n   = id m                    -- By law 2   \n   = m   \n \n"
  <> format "3.  (m >>= f) >>= g = m >>= (\\x -> f x >>= g)"
  <> par <> "Proof (recall that " <> fbox "fmap f . fmap g = fmap (f . g)" <> "):"
  <> format t
  <> par <> "These new monad laws, using " <> fbox "return" <> " and " <> formatSym "(>>=)" <> ", can be translated into do-block notation."
  <> format "\n "
  <> center tablaEqLaws
  <> format "\n "
  <> "The monad laws are now common-sense statements about how do-blocks should function. If one of these laws were invalidated, users would become confused, as you couldn't be able to manipulate things within the do-blocks as would be expected. The monad laws are, in essence, usability guidelines."
  where
    t =
      "    (m >>= f) >>= g                                                                               \n"
      <> "  = (join (fmap f m)) >>= g                          -- By the definition of (>>=)             \n"
      <> "  = join (fmap g (join (fmap f m)))                  -- By the definition of (>>=)             \n"
      <> "  = (join . fmap g) (join (fmap f m))                                                          \n"
      <> "  = (join . fmap g . join) (fmap f m)                                                          \n"
      <> "  = (join . join . fmap (fmap g)) (fmap f m)         -- By law 4                               \n"
      <> "  = (join . join . fmap (fmap g) . fmap f) m                                                   \n"
      <> "  = (join . join . fmap (fmap g . f)) m              -- By the distributive law of functors    \n"
      <> "  = (join . join . fmap (\\x -> fmap g (f x))) m                                               \n"
      <> "  = (join . fmap join . fmap (\\x -> fmap g (f x))) m -- By law 1                              \n"
      <> "  = (join . fmap (join . (\\x -> fmap g (f x)))) m    -- By the distributive law of functors   \n"
      <> "  = (join . fmap (\\x -> join (fmap g (f x)))) m                                               \n"
      <> "  = (join . fmap (\\x -> f x >>= g)) m                -- By the definition of (>>=)            \n"
      <> "  = join (fmap (\\x -> f x >>= g) m)                                                           \n"
      <> "  = m >>= (\\x -> f x >>= g)                          -- By the definition of (>>=)            \n"
