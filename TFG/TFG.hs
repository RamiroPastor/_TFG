{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Clock
import Data.Time.Calendar

import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSThm
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Hyperref

import Data.Text (pack)
import Dates (showDate)

import TFG_categorias (cap1_1, cap1_2)
import TFG_monadasHaskell (cap2)
import TFG_lastSteps (cap3)
import TFG_anexo1 (anexo1)
import TFG_anexo2 (anexo2)
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


-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
main :: IO ()
main = execLaTeXT tfg >>= renderFile "TFG.tex"

-- It's a good idea to separate the preamble of the body.
--simple :: Monad m => LaTeXT_ m
tfg = do
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
   author "Ramiro Pastor Martin"
   title "Monadnomicon"
   date fecha'



theBody :: LaTeXT IO ()
theBody = do
   maketitle
   -- section "Hello"
   -- "This is a simple example using the "
   -- hatex
   -- " library. "
   -- 'textbf' turns characters to bold font (as you already may know).
   -- textbf "Enjoy!"
   -- " "
   -- This is how we nest commands.
   -- textbf (large "Yoohoo!")
   -- "Monads originally come from a branch of mathematics called Category Theory. Fortunately, it is entirely unnecessary to understand category theory in order to understand and use monads in Haskell."
   -- "Monads are very useful in Haskell, but the concept is often difficult at first. Since they have so many applications, people often explain them from a particular point of view, and that can confuse your understanding of monads in their full glory."
   tableofcontents
   cap1_1
   cap1_2
   cap2
   cap3
   appendix
   anexo1
   anexo2
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
   chapter "FAQS"
   "Frequently Asked Questions, as found in high-quality webpages."
   section "Where does the term ``Monad'' come from?"
   section "A monad is just a monoid in the category of endofunctors, what's the problem?"
   section "How to extract value from monadic action?"
   section (raw "How is \\texorpdfstring{$<*>$} p pronounced?")
   section "Distinction between typeclasses MonadPlus, Alternative and Monoid?"
   section "Functions from `Alternative' type class"
   section "Confused by the meaning of the `Alternative' type class and its relationship with other type classes"
   section "What's wrong with GHC Haskell's current constraint system?"
   section "Lax monoidal functors with a different monoidal structure"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ1.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ2.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ3.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ3-1.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ4.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ5.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ6.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ7.pdf}"
   raw "\\cleardoublepage"
   raw "\\includepdf[pages=-]{FAQ8.pdf}"
   raw "\\cleardoublepage"
   




type LaTeXIO = LaTeXT IO

readIOString :: IO String -> LaTeXT IO ()
readIOString s = lift s >>= raw . pack


fecha = getCurrentTime >>= return . showDate . toGregorian . utctDay

fecha' = readIOString fecha 






