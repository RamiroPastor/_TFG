{-# LANGUAGE OverloadedStrings #-}
 



module TFG_anexo1 where


import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSThm
import Text.LaTeX.Packages.AMSFonts (mathbb, naturals)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Types


import TFG_categorias







anexo1 :: LaTeXC l => l
anexo1 =
   chapter "Appendix: The fundamental groupoid"
   <> "NOT directly RELATED TO MONAD"
   <> paragraph " " <> "Let X be a topological space. We define a category " <> math piX <> " (which will turn out to be a groupoid) as follows."
   <> (itemize objetosPiX) <> recordatorio <> (itemize morfismosPiX) <> introComposicion <> (itemize composicionPiX)
   <> theorem "prop" statements
   <> paragraph (textit "Def:") <> fundamentalGroupoid
   <> equivalentDefinitions
   <> ejemploUtilidad



-------- DEFINICIONES AUXILIARES --------

piX :: LaTeXC l => l
piX = (raw "\\Pi") <> "(X)"


intervalo_01 :: LaTeXC l => l
intervalo_01 = raw "[0,1]"


alfa :: LaTeXC l => l
alfa = alpha


-----------------------------------------


objetosPiX :: LaTeXC l => l
objetosPiX = item Nothing <> " The objects of " <> math piX <> " are the points of X. "


recordatorio :: LaTeXC l => l
recordatorio = "\n\n" <>  (par1 <> par2 <> proposicion)
   where
      par1 = 
         " In order to define morphisms in " <> math piX  <> " we need to recall the notion of " <> textit "homotopy of paths" <> "."
         <> " Suppose " <> math ("x,y" `in_` "X") <> " and " <> math ((gamma!:"0") <> "," <> (gamma!:"1")) <> " : " <> math (intervalo_01 <> to <> "X")
         <> " are continous maps (where the closed interval [0,1] is equipped with the usual topology) such that "
         <> math ( (gamma!:"0") <> "(0) = x = " <> (gamma!:"1") <> "(0)" ) <> " and " <> math ( (gamma!:"0") <> "(1) = y = " <> (gamma!:"1") <> "(1)" )
         <> ". (One can say that " <> math (gamma!:"0") <> " and " <> math (gamma!:"1") <> " are (continous) paths from x to y)."
      par2 = 
         "\n\n We say that " <> math (gamma!:"0") <> " and " <> math (gamma!:"1") <> " are " <> textbf "homotopic" <> " if there exists a continous map "
         <> math ( "H : " <> ((intervalo_01 `times` intervalo_01) <> to <> "X") ) <> ", called a " <> textbf "homotopy" <> " between " <> math (gamma!:"0") <> " and " <> math (gamma!:"1")
         <> ", such that " <> math ("H(t,0)" =: ((gamma!:"0") <> "(t)")) <> " and " <> math ("H(t,1)" =: ((gamma!:"1") <> "(t)")) <> ", " <> math (forall <> ("t" `in_` intervalo_01))
         <> ", and also " <> math ("H(0,s)" =: "x") <> " and " <> math ("H(1,s)" =: "y") <> ", " <> math (forall <> ("s" `in_` intervalo_01))
         <> ". Observe that these conditions can be rephrased as follows. If we define " <> math ( (("H"!:"s")<>"(t)") =: "H(t,s)" ) <> ", then, for every fixed " <> math ("s" `in_` "[0,1]")
         <> ", " <> math ("H"!:"s") <> " should be a (continous) path from x to y, and, moreover, one should have " <> math (("H"!:"0")=:(gamma!:"0")) <> " and " <> math (("H"!:"1")=:(gamma!:"1")) <> "."
      proposicion =
         "\n\n Check that being homotopic is an equivalence relation on continuous paths from x to y. This allows us to define, for every pair "
         <> math ("x,y" `in_` "X") <> ", the set of equivalence classes of continuous paths from x to y modulo homotopy."



morfismosPiX :: LaTeXC l => l
morfismosPiX = 
   item Nothing <> "For two objects " <> textit "x, y" <> " of " <> math piX <> ", we define " 
   <> math (homXY (Just piX) "x" "y") <> " to be the set of homotopy classes of continuous paths from " <>  textit "x" <> " to " <> textit "y" <> "."


introComposicion :: LaTeXC l => l
introComposicion =
   "Finally, we need to define composition of paths. If " <> (math alfa) <> " : " <> math (intervalo_01 <> to <> " X ") <> " and " 
   <> (math beta) <> " : " <> math (intervalo_01 <> to <> " Y ") <> " are continuous maps such that " <> math ((alfa <> "(1)") =: (beta <> "(0)")) <> ", we can define a continuous map"
   <> ecuacion
   <> "Check that composition of paths respects homotopy. In other words, if " <> (math (alfa!:"0")) <> " , " <> (math (alfa!:"1")) <> " are two homotopic paths from " <> math ("x" <> to <> "y")
   <> ", and " <> (math (beta!:"0")) <> " , " <> (math (beta!:"1")) <> " are two homotopic paths from " <> math ("y" <> to <> "z")
   <> ", then the paths " <> math ((beta!:"0") `cdot` (alfa!:"0")) <> " and " <> math ((beta!:"1") `cdot` (alfa!:"0")) <> " from " <> math ("x" <> to <> "z") <> " are also homotopic."


composicionPiX :: LaTeXC l => l
composicionPiX =
   item Nothing <> "This allows us to define composition of morphisms in " <> (math piX) <> " unambiguously: if " <> math ("x,y,z" `in_` "X") <> ", then to define the composition map"
   <> equation_ (((homXY Nothing "y" "z") `times` (homXY Nothing "x" "y")) <> to <> homXY Nothing "x" "z")
   <> "let us pick equivalence classes " <> math ("f" `in_` homXY Nothing "x" "y") <> " , " <> math ("g" `in_` homXY Nothing "y" "z") <> " and representatives "
   <> (math alfa) <> " : " <> math (intervalo_01 <> to <> "X") <> ", " <> (math beta) <> " : " <> math (intervalo_01 <> to <> "X") <> " of " <> math "f" <> " and " <> math "g"
   <> ", respectively. Then we define " <> math ("g" `circ` "f") <> " to be the equivalence class of " <> math (beta `cdot` alfa) <> "." 


ecuacion :: LaTeXC l => l
ecuacion = 
   equation_ ( (beta `cdot` alfa) <> " : " <> intervalo_01 <> to <> "X" <> ";" <> qquad <> "t" <> mapsto 
   <> raw "\\begin{cases} \\alpha (2t), & \\mbox{if } 0\\leq t\\leq \\frac{1}{2} \\\\ \\beta (2t-1) , & \\mbox{if } \\frac{1}{2}\\leq t\\leq 1 \\end{cases}" )



statements :: LaTeXC l => l
statements = "Verify the following statements:" <> enumerate ( st1 <> st2 <> st3)
   where
      st1 = item Nothing <> "Composition of homotopy classes of continuous paths is associative. (There is something to think about, because composition of continuous paths, before passing to homotopy classes, is NOT associative!)" 
      st2 = item Nothing <> "The definitions above turn " <> math piX <> " into a category."
      st3 = 
         item Nothing <> math piX <> " is in fact a groupoid. Indeed, if " <> math "f" <> " : " <> math ("x" <> to <> "y") <> " is a morphism in " <> math piX <> " and " <> math alfa <> " : " <> math (intervalo_01 <> to <> "X") 
         <>" is a representative of f, as before, check that the equivalence class of the path " <> math ((alfa^:"-1") <> " : " <> (intervalo_01 <> to <> "X")) <> " defined by " <> math (((alfa^:"-1")<>"(t)") =: (alfa<>"(1-t)")) <> " is an inverse of f."


fundamentalGroupoid :: LaTeXC l => l
fundamentalGroupoid  =
   "One calls " <> math piX <> " the " <> textbf "fundamental groupoid" <> " of the topological space " <> math "X" <> ". If we fix a point " <> math ("x" `in_` "X") <> ", then, in particular, all morphisms from " <> math "x" <> " to " <> math "x" <> " in " <> math piX
   <> " form a group which we denote " <> math (("Aut"!:piX)<>"(x)") <> ". In topology it has a diferent name: the " <> textit "fundamental group of X at the point x" <> " is defined to be " <> equation_ ((pi_!:"1")<>"(X,x)"<>":="<>("Aut"!:piX)<>"(x)") 


equivalentDefinitions :: LaTeXC l => l
equivalentDefinitions =
   "Check that this definition of the fundamental group is equivalent to the other one(s) you have seen. The proof will be essentially tautological. The definition of the fundamental groupoid is no more complicated than the definition of the fundamental group;"
   <> " however, for many purposes it is much more convenient to think in terms of the fundamental groupoid rather than the fundamental group."


ejemploUtilidad :: LaTeXC l => l
ejemploUtilidad =
   paragraph " " <> "For example, the definition of the fundamental groupoid is completely canonical, while the definition of the fundamental group depends on the choice of a base point. In particular, if " <> math "X" <> " has several connected components, then "
   <> math piX <> " " <> qts "keeps track" <> " of all of them, while if we choose a base point " <> math ("x" `in_` "X") <> ", then " <> math ((pi_!:"1")<>"(X,x)") <> " does not know anything about the connected components of " <> math "X" <> " that do not contain " <> math "x" <> "."
   <> " For instance, if " <> math "X" <> " is the disjoint union of a circle and a line, and " <> math ("x" `in_` "X") <> " is a point lying on the line, then " <> math ((pi_!:"1")<>"(X,x)") <> " is the trivial group, while " <> math piX
   <> " looks like the " <> qts "disjoint union" <> " (you can try to think how to define the disjoint union of two categories in general - this is not dificult) of the fundamental groupoid of a circle and the fundamental groupoid of a line."

















