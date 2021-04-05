{-# LANGUAGE OverloadedStrings #-}




module TFG_ejerciciosResueltos where





import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath


import TFG_maybeMonad (maybeMonad, bind)
import TFG_monadasHaskell (appMaybe, apply)
import TFG_ejercicios
import TFG_comandoReadCode




-------- SIMBOLOS U OTRO CODIGO --------

format :: LaTeXC l => String -> l
format = verbatim . pack

funcType :: LaTeXC l => l
funcType = fbox ( math ( "((" <> to <> ")"<>(raw "\\;")<>"r)" ) )

formatSymbol :: LaTeXC l => l -> l
formatSymbol = scriptsize . fbox . math


----------------------------------------

ejsResueltos :: LaTeXT IO ()
ejsResueltos = 
  chapter "My solutions for the exercises" 
  >> "Solutions will be given in packs, one for each section"
  >> section ( "Basic " <> textit "Functor" <> " and " <> textit "Applicative" <> " solutions")
  >> enumerate (ejercicio1 <> ejercicio2 <> ejercicio3)
  >> newpage
  >> sol1
  >> newpage  
  >> section ( "Advanced " <> textit "Monad" <> " and " <> textit "Applicative" <> " solutions" )
  >> enumerate (ejercicio4 <> ejercicio5 <> ejercicio6 <> ejercicio7 <> ejercicio8 <> ejercicio9 <> ejercicio10 <> ejercicio11 <> ejercicio12)
  >> par <> "The next few exercises concern the following tree data structure:"
  >> footnote ( "In case you are wondering, " <> qts "AT" <> " stands for " <> qts "apple tree" <> "." ) 
  >> verbatim "  data AT a = L a | B (AT a) (AT a)  \n"
  >> enumerate ("[resume]" <> ejercicio13 <> ejercicio14 <> ejercicio15 <> ejercicio16 <> ejercicio17 <> ejercicio18)
  >> newpage
  >> sol2
  >> newpage
  >> section ( textit "State" <> " exercises" )
  >> enumerate (ejercicio19 <> ejercicio20 <> ejercicio21 <> ejercicio22 <> ejercicio23 <> ejercicio24)
  >> newpage
  >> sol3
  >> newpage
  >> section ( textit "MonadPlus" <> " exercises" )
  >> enumerate (ejercicio25 <> ejercicio26)
  >> newpage
  >> sol4
  >> newpage
  >> section "Monad transformers exercises'"
  >> enumerate (ejercicio27 <> ejercicio28 <> ejercicio29 <> ejercicio30)
  >> newpage  
  >> sol5
  >> newpage
  >> section "Hask category exercises"
  >> enumerate (ejercicio31 <> ejercicio32 <> ejercicio33 <> ejercicio34 <> ejercicio35 <> ejercicio36 )
  >> newpage
  >> sol6


sol1 = readCode "ejercicios_resueltos/BasicFunctorAndApp.hs"

sol2 = readCode "ejercicios_resueltos/AdvancedApplicative.hs"

sol3 = readCode "ejercicios_resueltos/StateExercises.hs"

sol4 = readCode "ejercicios_resueltos/MonadPlusExercises.hs"

sol5 = readCode "ejercicios_resueltos/MonadTransformers.hs"

sol6 = readCode "ejercicios_resueltos/HaskCategory.hs"

