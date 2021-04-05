{-# LANGUAGE OverloadedStrings #-}


module TFG_anexo3 where


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSThm

import Data.Text (pack)




anexo3 :: LaTeXC l => l
anexo3 = 
  chapter "Appendix: the Monoid type class"
  <> "Not to be confused with the Monad class, the more pleasant Monoid class, with kind " <> verb " * -> Constraint " <> ", found in the " <> fbox "Data.Monoid" <> " module, modelizes the semigroups or monoids."
  <> par <> "A " <> textbf "monoid" <> " in Mathematics is an algebraic structure consisting of a set of objects with an operation between them, being this operation "
  <> textit "associative" <> " and with a " <> textit "neutral element" <> ".  Phew! But what is the meaning of this? "
  <> "By " <> textit "associative" <> " we mean that, if you have three elements " <> math "a" <> ", " <> math "b" <> " and " <> math "c" 
  <> ", then " <> math "a*(b*c) = (a*b)*c" <> ". A " <> textit "neutral element" <> " is the one that does not worth to operate with, because it does nothing! "
  <> "To say, " <> math "e" <> " is a " <> textit "neutral element" <> " if " <> math "e*a = a*e = a" <> ", given any object " <> math "a"
  <> ". As an example, you may take the real numbers as objects and the ordinary multiplication as operation."
  <> par <> "Now that you know the math basics behind the Monoid class, let's see its definition:"
  <> definicionMonoid
  <> par <> "See that " <> textbf "mappend" <> " corresponds to the monoid operation and " <> textbf "mempty" <> " to its neutral element. The names of the methods may seem unsuitable"
  <> ", but they correspond to an example of monoid: the lists with the appending " <> verb "(++)"  <> " operation. Who is the neutral element here? The empty list:"
  <> verbatim (pack "    xs ++ [] = [] ++ xs = xs")
  <> par <> "Some examples:" <> newpage
  <> ejemplos
  <> par <> "As you can see in all the examples, the following rules are verified:"
  <> verbatim (pack reglasMonoide)




definicionMonoid :: LaTeXC l => l
definicionMonoid = verbatim (pack t) where
  t =
    "  class Monoid m where \n " <>
    "    mempty :: m \n " <>
    "    mappend :: m -> m -> m \n " <>
    "    mconcat :: [m] -> m \n " <>
    "    mconcat = foldr mappend mempty \n " <>
    "    (<>) :: m -> m -> m    -- infix synonym for mappend "




reglasMonoide :: String
reglasMonoide =
  "  (x <> y) <> z = x <> (y <> z)   -- associativity \n"
  <> "  mempty <> x = x                 -- left identity \n"
  <> "  x <> mempty = x                 -- right identity \n"




ejemplos :: LaTeXC l => l
ejemplos = ej1 <> ej2 <> ej3 <> ej4 <> ej5 <> ej6
  where
    ej1 = par <> "The list monoid" <> verbatim (pack listMonoid)
    listMonoid =  "instance Monoid [a] where  \n " <> "        mempty  = [] \n " <> "        mappend = (++) \n " <> "        mconcat xss = [x | xs <- xss, x <- xs] \n "
    
    ej2 = par <> "The monoid of functions with range a monoid" <> verbatim (pack listFun)
    listFun = "instance Monoid b => Monoid (a -> b) where \n " <> "        mempty _ = mempty \n " <> "        mappend f g x = f x `mappend` g x \n "
    
    ej3 = par <> "The Unit monoid" <> verbatim (pack unitMonoid)
    unitMonoid = "instance Monoid () where \n " <> "        mempty        = () \n " <> "        _ `mappend` _ = () \n " <> "        mconcat _     = () \n "
    
    ej4 = par <> "The cartesian product of two monoids" <> verbatim (pack productMonoid)
    productMonoid = "instance (Monoid a, Monoid b) => Monoid (a,b) where \n " <> "        mempty = (mempty, mempty) \n " <> "        (a1,b1) `mappend` (a2,b2) = \n " <> "                (a1 `mappend` a2, b1 `mappend` b2) \n "

    ej5 = par <> "Lexicographical ordering" <> verbatim (pack lexOrd)
    lexOrd = "instance Monoid Ordering where \n " <> "        mempty         = EQ \n " <> "        LT `mappend` _ = LT \n " <> "        EQ `mappend` y = y \n " <> "        GT `mappend` _ = GT \n "

    ej6 = par <> "Lift a semigroup into `Maybe' forming a `Monoid'" <> verbatim (pack maybeMonoid)
    maybeMonoid = "instance Monoid a => Monoid (Maybe a) where \n " <> "  mempty = Nothing \n " <> "  Nothing `mappend` m = m \n " <> "  m `mappend` Nothing = m \n " <> "  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2) \n"











