{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Calendar
import Dates (showDate)

import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSThm
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Hyperref

import TFG_categorias (cap1_1, cap1_2)
import TFG_monadasHaskell (cap2)
import TFG_lastSteps (cap3)
import TFG_anexo3 (anexo3)
import TFG_maybeMonad (anexo4)
import TFG_listMonad (anexo5)
import TFG_IOMonad (anexo6)
import TFG_IOlibrary (anexo7)
import TFG_StateMonad (anexo8)
import TFG_System_Random (anexo9)
import TFG_anexoClave (resumenClave)
import TFG_ejercicios (enunciados)
import TFG_ejerciciosResueltos (ejsResueltos)

import TFG_comandoChangeMargin

--------------------------------------------------------------------------------

main :: IO ()
main = execLaTeXT memo >>= renderFile "Memoria.tex"
   where
      memo = do
        thePreamble
        document theBody


thePreamble :: LaTeXT IO ()
thePreamble = do
   documentclass [openright, titlepage, twoside] report
   usepackage [] amsmath
   usepackage [] amsfonts
   usepackage [] amsthm
   usepackage [] graphicx
   usepackage [] hyperref
   importGeometry [ GPaper A4, GHeight (Cm 22) , GWidth (Cm 15) ]
   raw "\\usepackage{tikz-cd}"
   raw "\\usepackage{enumitem}"
--   raw "\\usepackage{pbox}"
   raw "\\usepackage[final]{pdfpages}"
   cambiarMargenesCMD
   author "Ramiro Pastor Martín"
   title "Haskell Monads"
   date $ lift (showDate . toGregorian . utctDay <$> getCurrentTime) >>= raw . pack


portada :: LaTeXT IO ()
portada =
      raw (pack t)
   where
      t =   "\\makeatletter"
         <> "\\begin{titlepage} "
         <> "\\centering "
         <> "{\\huge\\bfseries \\@title \\par}"
         <> "\\vspace{1.5cm}"
         <> "{\\scshape\\large Bachelor's Degree Thesis\\par}"
         <> "\\vspace{1cm}"
         <> "{\\scshape\\large Academic year 2019/2020} \\par"
         <> "\\vspace{1.5cm}"
         <> "\\includegraphics[width=0.8\\textwidth]{ucmlogo.jpg}\\par\\vspace{1cm}"
         <> "{\\scshape\\Large Faculty of Mathematics \\par}"
         <> "{\\large Mathematics Degree \\par}"
         <> "\\vspace{2.5cm}"
         <> "{\\large Student: \\itshape \\@author \\par}"
         <> "\\vfill \n"
         <> "supervised by\\par "
         <> "Luis Fernando Llana Díaz"
         <> "\\vfill"
         <> "{\\large \\@date \\par}"
         <> "\\end{titlepage}"



theBody :: LaTeXT IO ()
theBody = do
  portada
  tableofcontents
  intro
  cap1_1
  cap1_2
  cap2
  cap3
  conclusion
  biblio
  appendix
  anexo3
  anexo4
  anexo5
  anexo6
  anexo7
  anexo8
  anexo9
  resumenClave
  enunciados
  ejsResueltos


--------------------------------------------------------------------------------

intro :: LaTeXT IO ()
intro = do
  chapter "Introduction"
  section "Motivation"
  paragraph ""
  "Monads are an essential part of Haskell, starting with input/output. All "
  "I/O operations, from reading or writing to the console to editing files, are "
  "monadic actions in the IO monad. For example, reading a value from the "
  "console returns a type 'IO a' and there is no safe way to extract the value "
  "of type 'a' outside the IO type, thus it is mandatory to use the monad "
  "operators and techniques to operate with that value."
  paragraph ""
  "Furthermore, the monad typeclass is intimately related to the 'do' notation "
  "which enables imperative programming in Haskell, a declarative language. "
  "This means that anytime you have a monad, you can write do blocks, switching "
  "to imperative programming. This applies to every monad, from the IO type to "
  "all other monadic types, for example, do blocks can return HTML types, HTTP "
  "actions, or even LaTeX documents. The rules for do blocks are pretty simple "
  "so it is easy to sequence statements in the corresponding monadic type. "
  "Finally, whenever more than one monad are involved, they can be mixed with "
  "the 'lift' or the 'liftIO' functions, which correspond to the monad "
  "transformers typeclasses."
  section "Objectives"
  paragraph ""
  "The aim of this work is to explain the Monad typeclass of Haskell. "
  "It begins with a brief introduction to Category Theory, from where the "
  "concept of Monad is taken; however, it does not delve into the theory "
  "too much, because it is not a prerequisite to learn about Haskell's "
  "monads, which can be used without any knowledge of Category Theory. "
  "It is there merely for completition purposes (and to expose where "
  "the term 'monad' comes from)"
  paragraph ""
  "Chapter two is dedicated to Haskell entirely, introducing the Monad "
  "typeclass and its superclasses Functor and Applicative, explaining the "
  "'do' notation (a tool associated with monads), and ending with the "
  "MonadPlus typeclass and Monad Transformers."
  paragraph ""
  "Chapter three revisits the Functor, Applicative and Monad typeclasses to "
  "expose the similarities between them, and links with the first "
  "chapter thanks to the 'Hask' category. \n"
  "Finally, there are some appendixes which include exercises, random "
  "modelization in Haskell and more."


conclusion :: LaTeXT IO ()
conclusion = do
  chapter "Conclusion"
  paragraph ""
  "To wrap everything up: we learned the basics of some essential Haskell typeclasses, "
  "from the simple Monoid typeclass to the central piece of the report, the "
  "Monad typeclass, not forgetting Functor, Applicative, MonadPlus and monad "
  "transformers; and we have also seen how it applies to different datatypes, "
  "such as the Maybe type, the IO type or even lists."
  paragraph ""
  "We also learned the mechanics of the very useful do notation, which enables "
  "imperative programming in Haskell; and reviewed the basics of Category Theory "
  "tiding everything up with the Hask category. Moreover, the rules for each "
  "of these typeclasses were also presented."
  paragraph ""
  "In conclusion, we can now apply this knowledge to our every day Haskell "
  "programming, and hopefully feel that monads have been demystified for us. "
  "Just remember, monad is to one what triad is to three!"



biblio :: LaTeXT IO ()
biblio = do
  chapter "Bibliography"
  paragraph $ do
    "Saunders Mac Lane. Categories for the Working Mathematician. "
    "University of Chicago, Springer, 1998"
  paragraph $ do
    "Steve Awodey. Category Theory. Oxford University, "
    "Oxford University Press, 2010"
  paragraph $ do
    "B. O’Sullivan, D. Stewart, and J. Goerzen. Real world Haskell. "
    "O’Reilly, 2008"
  paragraph $ do
    "B. C. Ruiz, F. Gutiérrez, P. Guerrero, and J. Gallardo. "
    "Razonando con Haskell (Un curso sobre programación funcional). "
    "Thompson, 2004. "
  paragraph $ do
    "José A. Alonso Jiménez, Ma José Hidalgo Doblado. "
    "Piensa en Haskell (Ejercicios de programación funcional con Haskell). "
    "Universidad de Sevilla, 2012"


--------------------------------------------------------------------------------
