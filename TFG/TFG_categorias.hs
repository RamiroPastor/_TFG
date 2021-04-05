{-# LANGUAGE OverloadedStrings #-}

module TFG_categorias where


import Data.Text (pack)

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSThm
import Text.LaTeX.Packages.AMSFonts (mathbb, naturals)




simCat :: LaTeXC l => String -> l
-- para escribir la letra de una categoria en bonito
simCat s = raw ( "\\mathfrak{" <> (pack s) <> "}" )


objCat :: LaTeXC l => l -> l
-- para escribir Obj(c) dada una categoria c
objCat c = "Obj(" <> c <> ")"


homXY :: LaTeXC l => Maybe l -> l -> l -> l
-- para escribir Hom(X,Y) o tambien Hom_subC_(X,Y)
homXY Nothing    obj1 obj2 = "Hom" <> "(" <> obj1 <> ", " <> obj2 <> ")"
homXY (Just cat) obj1 obj2 = ("Hom" !: cat) <> "(" <> obj1 <> ", " <> obj2 <> ")"


endomorfX :: LaTeXC l => Maybe l -> l -> l
-- para escribir Endo(X) o Endo_subC_(X)
endomorfX Nothing  obj = "Endo(" <> obj <> ")"
endomorfX (Just c) obj = ("Endo" !: c) <> "(" <> obj <> ")"


hask :: LaTeXC l => l
hask = math (simCat "Hask")


simC :: LaTeXC l => l
simC = simCat "C"


simC' :: LaTeXC l => l
simC' = math (simCat "C")


circ' :: LaTeXC l => l
circ' = circ " " " "


psiDfiDX :: LaTeXC l => l
psiDfiDX =  psiu<>autoParens (phiu<>"(X)")

psiDfiDY :: LaTeXC l => l
psiDfiDY =  psiu<>autoParens (phiu<>"(Y)")

psiDfiDf :: LaTeXC l => l
psiDfiDf =  psiu<>autoParens (phiu<>"(f)")


introduction :: Monad m => LaTeXT_ m
introduction = do
  "This is a very brief introduction to Category Theory. It features the "
  "definition of category, functor and natural transformation, as well as a "
  "quick insight into monads, without going into details - it is merely being "
  "exposed for completition purposes."


cap1_1 :: Monad m => LaTeXT_ m

cap1_1 = do
   raw "\\newcommand\\myeq{\\mathrel{\\overset{\\makebox[0pt]{\\mbox{\\normalfont\\tiny\\sffamily associativity}}}{\\longrightarrow}}}"
   raw "\\newcommand\\myarrow{\\mathrel{\\overset{\\makebox[0pt]{\\mbox{\\normalfont\\tiny\\sffamily $\\simeq$}}}{\\rightarrow}}}"
   raw "\\newcommand{\\powerset}{\\raisebox{.15\\baselineskip}{\\Large\\ensuremath{\\wp}}} "
--   new theorem "def" (textit "Def:")
   newtheorem "prop" (textit "Prop:")
   newtheorem "obs" (textit "Obs:")
   chapter "Introduction to Categories"
   introduction
   section "Category"
   subsection "Definition"
   textit (textbf "Def:")
   " a "
   textbf "category "
--   let simC = simCat "C"
   let objC = objCat simC
   let homC = "Hom(" >> simC >> ")"
--   let simC' = math simC
   let objC' = math objC
   let homC' = math homC
   let homXYsubC = homXY (Just simC) "X" "Y"
   let homXXsubC = homXY (Just simC) "X" "X"
   let endoX = endomorfX (Just simC) "X"
   let homXZ = homXY (Just simC) "X" "Z"
   let homYZ = homXY (Just simC) "Y" "Z"
   let odot' = odot " " " "
   " is a tern "
   math (mathbf (langle >> objC >> ", " >> homC >> ", " >> (odot " " " ") >> rangle))
   " where : "
   let
      punto1 =
         item Nothing >> objC' >> " is a class (not necessarily a set) whose members are called " >> textbf "objects" >> " of " >> simC'
         >> ". In practice one often abuses notation by denoting the class of objects of " >> simC' >> " by the letter " >> simC'
         >> " as well. In particular, the notation " >> math ("X" `in_` simC) >> " is to be understood as " >> qts ("X is an object of " >> simC' ) >> ". "
      punto2 =
         item Nothing >> homC' >> " is a class (if " >> objC' >> " is a class) or a set (if " >> objC' >> " is a set) whose members are called " >> textbf "morphisms" >> " of " >> simC' >> "."
         >> " Each morphism f of " >> simC' >> " is associated with a " >> textbf "departure object" >> " X, and an " >> textbf "arrival object" >> " Y, both from " >> objC'
         >> "; we write this as " >> qts "f goes from X to Y" >> " or " >> math ("f : X " >> to >> " Y") >> " or " >> math ("X " >> raw "\\xrightarrow{f}" >> "Y")
         >> ". \n\nThe (always a) set of morphisms from X to Y in the category " >> simC' >> " is denoted as " >> math ( mathbf homXYsubC )
         >> ". Also, instead of " >> math homXXsubC >> " we will write " >> math endoX >> "; its elements are called " >> textbf "endomorphisms" >> " of X."
      punto3 =
         item Nothing >> "A composition law " >> math (odot " " " " ) >> " that " >> math forall >> " " >> math (" X, Y, Z" `in_` simC) >> ":"
         >> center (math ((times homXYsubC homYZ) >> to >> homXZ))  >> center (math (" (f,g) " >> mapsto >> (odot "g" "f")))
         >> "this is, for every " >> math ("X " >> raw "\\xrightarrow{f}" >> "Y" >> raw "\\xrightarrow{g}" >> "Z") >> " there must exist a morphism " >> math ("h : X " >> to >> " Z") >> " assigned to " >> math (odot "g" "f") >> ". It must verify:"
         >> paragraph "Associativity" >> "Composition of morphisms is associative. More precisely, given objects X, Y, Z, W of " >> simC' >> " and morphisms " >> math ("X " >> raw "\\xrightarrow{f}" >> "Y" >> raw "\\xrightarrow{g}" >> "Z" >> raw "\\xrightarrow{h}" >> "W")
         >> " we require that " >> math ( (odot "h" (autoParens (odot "g" "f"))) =: (odot (autoParens (odot "h" "g")) "f"))
         >> paragraph "Neutral elements" >> "Every object has an " >> qts "identity endomorphism" >> ". More precisely, if " >> math ("X" `in_` simC) >> ", there exists an element " >> math (("id"!:"X") `in_` endoX) >> " such that for every morphism " >> math ("f : X " >> to >> " Y") >> " in " >> homC'
         >> ", we have " >> math ((odot "f" ("id"!:"X")) =: "f") >> ", and for every morphism " >> math ("g : Z " >> to >> " X") >> " in " >> homC' >> ", we have " >> math ((odot ("id"!:"X") "g" ) =: "g") >> "."
   enumerate (punto1 >> punto2 >> punto3)
   --------
   theorem "obs" ( " In view of the associative axiom, whenever we have any composable sequence " >> math (("f"!:"1") >> "," >> ldots >> "," >> ("f"!:"n"))
      >> " of morphisms in a category, the expression " >>  math (("f"!:"n") >> odot' >> ("f"!:"n-1") >> odot' >> ldots >> odot' >> ("f"!:"2") >> odot' >> ("f"!:"1")) >> " is unambiguous.")
   --------
   subsection "Unicity of neutral elements and examples"
   theorem "prop" (" For any category " >> simC' >> " and any object " >>  math ("X" `in_` simC) >> ", there is only one endomorphism of X satisfying the defining property of " >> math ("id"!:"X") >> ". Thus one can really speak of the identity endomorphism of X.")
   let
      idXhat   = hat ("id"!:"X")
      idXtilde = tilde ("id"!:"X")
      eq1 = ((odot idXtilde "f") & (" " =: " f")) & ((odot idXhat "f") & (" " =: "f"))
      eq2 = ((odot "g" idXtilde) & (" " =: " g")) & ((odot "g" idXhat) & (" " =: "g"))
      p = {-indent >> textit "Dem:"-}
         " Given " >> math ("X" `in_` simC) >> " suppose that there are two endomorphisms of X, " >> math (widetilde ("id"!:"X")) >> " and " >> math (widehat ("id"!:"X")) >> ", with the property of neutral element. \n\n"
         >> "This is, " >> math forall >> " " >> math ("f" `in_` (homXY Nothing "A" "X")) >> " and " >> math forall >> " " >> math ("g" `in_` (homXY Nothing "X" "B")) >> " occurs: "
         >> align_ [eq1,eq2]
         >> "If we apply this to " >> math (odot idXhat idXtilde) >> " it falls that:"
         >> equation_ (( idXhat =: (idXhat `odot` idXtilde) =: idXtilde) >> implies >> (idXhat =: idXtilde))
   proof Nothing p
   textit "Examples" >> ": (note: " >> math odot' >> " is always the usual function composition " >> math (circ " " " ") >> " unless said otherwise.)"
   let
      set = math (simCat "Set")
      grp = math (simCat "Grp")
      top = math (simCat "Top")
      vect = math ((simCat "Vect") !: (mathbb "K"))
      menorOigual = " " <=: " "
      ej1 = item (Just set) >> " : " >> objCat set >> " -- the class of all sets. " >> "Hom(">> set >> ")" >> " -- functions between sets."
      ej2 = item (Just grp) >> " : " >> objCat grp >> " -- the class of all groups. " >> "Hom(">> grp >> ")" >> " -- group homomorphisms."
      ej3 = item (Just top) >> " : " >> objCat top >> " -- the class of all topological spaces. " >> "Hom(">> top >> ")" >> " -- continuous maps between topological spaces."
      ej4 = item (Just vect) >> " : " >> objCat vect >> " -- the class of all vector spaces over a given field " >> math (mathbb "K")>> ". " >> "Hom(">> vect >> ")" >> " -- linear maps between vector spaces."
      ej5 = item (Just hask) >> " : " >> objCat hask >> " -- the class of all Haskell types. " >> "Hom(">> hask >> ")" >> " -- Haskell functions." >> " The composition law is the " >> math (mathtt "(.)") >> " operator."
      ej6 =
         item (Just (math (mathbf menorOigual))) >> " : any partially ordered set " >> math (langle >> "P" >> ", " >> menorOigual >> rangle) >> " defines a category where the objects are the elements of P, and there is a morphism (and only one) between any two objects A and B iff " >> math ("A" <=: "B")
         >> ".\n\n This can be applied to the power set " >> math (raw "\\powerset") >> "(X)" >> " of any given set X, taking the inclusion as the partial order."
   enumerate (ej1 >> ej2 >> ej3 >> ej4 >> ej5 >> ej6 )
   --------
   subsection "Isomorphisms and Automorphisms"
   textit (textbf "Def:") >> " Let " >> simC' >> " be a category and " >> math ("X " >> raw "\\xrightarrow{f}" >> "Y") >> " a morphism in " >> simC' >> ". We say that f is an " >> textbf "isomorphism" >> ", or that f is " >> textbf "invertible" >> ", if there exists a morphism "
   >> math (("g : Y " >> to >> " X")) >> " in " >> math (simCat "C") >> " such that " >> math (("g" `odot` "f") =: ("id"!:"X")) >> " and " >> math (("f" `odot` "g") =: ("id"!:"Y")) >> ". If this is the case, g is called an " >> textbf "inverse" >> " of f, and viceversa."
   >> theorem "prop" ("If f has an inverse, that inverse is unique (so, if f is an isomorphism, one can unambiguously denote its inverse by " >> math ("f"^:"-1")  >> ").")
   >> proof Nothing ("given " >> math ("f : X " >> to >> " Y") >> " such that exists " >> math ("g : Y " >> to >> " X") >> " and " >> math ("g*: Y" >> to >> " X") >> " inverses of f, lets conclude that g = g*. We have:"
      >> align_ [ ((odot "g" "f") & (" " =: ("id"!:"X"))) & ((odot "f" "g") & (" " =: ("id"!:"Y"))) , ((odot "g*" "f") & (" " =: ("id"!:"X"))) & ((odot "f" "g*") & (" " =: ("id"!:"Y"))) ]
      >> "beginning with the first ecuation, and composing with g* to the right, we have " >> math (("g" `odot` "f" `odot` "g*") =: (("id"!:"X") `odot` "g*")) >> " and applying the last ecuation, we get "
      >> math ( ("g" `odot` ("id"!:"Y")) =: (("id"!:"X") `odot` "g*") >> implies >> ("g"=:"g*")) )
   >> theorem "prop" ("If " >> math ("f : X " >> to >> " Y") >> " and " >> math ("g : Y " >> to >> " Z") >> " are composable morphisms and are both invertible, then " >> math (odot "g" "f") >> " is also invertible, and " >> math (((autoParens ("g" `odot` "f"))^:"-1") =: (("f"^:"-1") `odot` ("g"^:"-1"))) >> "." )
   >> proof Nothing ("the existence of " >> math ((autoParens ("g" `odot` "f"))^:"-1") >> " will be given automatically as soon as we prove that " >> math (((autoParens ("g" `odot` "f"))^:"-1") =: (("f"^:"-1") `odot` ("g"^:"-1"))) >> " because we know that " >> math (("f"^:"-1") `odot` ("g"^:"-1")) >> " must exist."
      >> "\n\n In addition, with the already proved uniqueness of inverses (last proposition) and its consequent unambiguity in the use of the " >> math ("f"^:"-1") >> " notation, we only need to prove that composing the function " >> math ("g" `odot` "f") >> " with the function " >> math (("f"^:"-1") `odot` ("g"^:"-1"))
      >> " gives us the indentity function in both X and Z. Indeed: "
      >> align_ [ (autoParens ("g" `odot` "f")) `odot` ("f"^:"-1") `odot` ("g"^:"-1") & " = " & raw "\\myeq" & " = " & "g" `odot` (autoParens ("f" `odot` ("f"^:"-1"))) `odot` ("g"^:"-1") & (" " =: " ") & ("id"!:"Z") , (("f"^:"-1") `odot` ("g"^:"-1") `odot` autoParens ("g" `odot` "f")) & " = " & raw "\\myeq" & " = " & ("f"^:"-1") `odot` (autoParens (("g"^:"-1") `odot` "g")) `odot` "f" & (" " =: " ") & ("id"!:"X")] )
   >> theorem "obs" ("the reciprocal proposition (invertible composition implies invertible factors) is not true in general. As a counterexample, in the " >> math (simCat "sets") >> " category take f,g : " >> math (naturals >> to >> naturals) >> " with " >> math "f(x)=2x" >> " and " >> math ("g(x)=" >> lfloor >> "x/2" >> rfloor) >> ", and compose " >> math ("g" `circ` "f") >> ".")
   >> paragraph (textit "Def:") >> " If X, Y are objects of a category " >> math (simCat "C") >> " such that there exists an isomorphism (i.e. invertible) " >> math ("f: X" >> to >> " Y") >> ", we say that X and Y are " >> textbf "isomorphic" >> ", and write " >> math ("X " >> (raw "\\cong") >> " Y") >> " or " >> ("f : X " >> math (raw "\\myarrow") >> " Y") >> "."
   >> theorem "obs" (" Usually, if an isomorphism between X and Y exists, it is non-unique. For example, there exists a bijection between two finite sets " >> math (raw "\\leftrightarrow") >> " both have the same number n of elements, and in that case there are exactly n! bijections between them.")
   >> paragraph (textit "Def:") >> " If " >> math (simCat "C") >> " is a category and " >> math ("X" `in_` (simCat "C")) >> ", the invertible endomorphisms of X are called " >> textbf "automorphisms" >> "."
   >> paragraph (textit "Def:") >> " If " >> math (simCat "C") >> " is a category and " >> math ("X" `in_` (simCat "C")) >> ", the set of all invertible endomorphisms of X will be denoted by " >> math (mathbf (("Aut"!:(simCat "C"))>>"(X)")) >> "."
   >> (theorem "prop" ( (math (("Aut"!:(simCat "C"))>>"(X)")) >> " is a group under the composition of morphisms. It is called the " >> textit "group of automorphisms" >> " of X." ))
   --------
   >> subsection ( "Groupoids and Subcategories" )
   >> paragraph (textit "Def:") >> " A " >> textbf "groupoid" >> " is a category in which every morphism is invertible."
   >> paragraph (textit "Def:") >> " Let " >> math (simCat "C") >> " be any category, and let " >> math (mathbf ((simCat "C")^:"x")) >> " be the category whose class of objects is that of " >> math (simCat "C") >> ", and where the morphisms are defined as follows."
   >> " If X, Y are two objects, then "  >> math (homXY (Just ((simCat "C")^:"x")) "X" "Y") >> " is the set of invertible elements of " >> math (homXY (Just (simCat "C")) "X" "Y") >> ". The composition of morphisms in " >> math ((simCat "C")^:"x") >> " is defined as the composition in " >> math (simCat "C") >> "."
   >> theorem "prop" ( math ((simCat "C")^:"x") >> " is a category and is a groupoid." )
   >> paragraph (textit "Def:") >> " Let " >> simC' >> " be a category. We say that a category " >> math (simCat "D") >> " is a " >> textbf "subcategory" >> " of " >> simC' >> " if: the class of objects of " >> math (simCat "D") >> " is a subclass of the class of objects of " >> simC' >> " , i.e. " >>  math ((objCat (simCat "D")) `subset` (objCat simC))
   >> "; for every pair X, Y of objects of " >> math (simCat "D") >> ", the set " >> math (homXY (Just (simCat "D")) "X" "Y") >> " is a subset of " >> math (homXY (Just simC) "X" "Y") >> "; and composition of two morphisms in " >> math (simCat "D") >> " is the same regardless of whether it is computed in " >> math (simCat "D") >> " or in " >> simC' >> ".\n\n"
   >> " We say that " >> math (simCat "D") >> " is a " >> textbf "full subcategory" >> " of " >> simC' >> " if, for every pair of objects X, Y of " >> math (simCat "D") >> ", one has " >> math ((homXY (Just (simCat "D")) "X" "Y") =: (homXY (Just simC) "X" "Y")) >> " (this is, we only lose objects and not morphisms)."



cap1_2 :: LaTeXC l => l
cap1_2 =
   newpage <> section "Functors and natural transformations" <> " From now on, the symbol " <> math (odot " " " ") <> " will be replaced by " <> math (circ " " " ")
   <> subsection "Definition" <> textit (textbf "Def:") <> " Let " <> math (simC!:"1") <> " and " <> math (simC!:"2") <> " be categories. A " <> textbf "covariant functor " <> math (phiu <> " : " <> (simC!:"1") <> to <> (simC!:"2")) <> " is a rule which to every object " <> math "X" <> " of " <> math (simC!:"1") <> " assigns an object "
   <> math (phiu <> "(X)") <> "  of " <> math (simC!:"2") <> ", and to every morphism " <> math ("f : X" <> to <> "Y") <> " in " <> math (simC!:"1") <> " assigns a morphism " <> math (phiu<>"(f)"<>" : "<>phiu<>"(X)"<>to<>phiu<>"(Y)" ) <> " in " <> math (simC!:"2") <> " such that " <> math (phiu<>"("<>("g"`circ`"f")<>")"<>" = "<>((phiu<>"(g)")`circ`(phiu<>"(f)")) )
   <> " whenever " <> math ("X " <> raw "\\xrightarrow{f}" <> "Y" <> raw "\\xrightarrow{g}" <> "Z") <> " are morphisms in " <> math (simC!:"1") <> ", and such that " <> math (phiu<>"("<>("id"!:"X")<>")"<>" = "<> ("id"!:phiu) <>"(X)" ) <> " for all objects " <> math "X" <> " of " <> math (simC!:"1") <> "."
   <> paragraph " " <> " To get the notion of a " <> textbf "contravariant functor " <> math (psiu<>" : "<>(simC!:"1")<>to<>(simC!:"2")) <> " one has to make the following changes: " <> math (psiu<>"(f)") <> " should now be a morphism from " <> math (psiu<>"(Y)") <> " to " <> math (psiu<>"(X)")
   <> " (i.e., " <> math psiu <> qts "reverses the directions of all arrows" <> "), and the first requirement in the definition of a functor has to be replaced by " <> math ( (psiu<>("(g"`circ`"f)")) =: ((psiu<>"(f)")`circ`(psiu<>"(g)")) ) <> "."
   <> paragraph " " <> " Usually, the word " <> qts "functor" <> " without any adjectives refers to a covariant functor. I will also use this convention from now on."
   --------
   <> subsection "Examples of functors" <> " There are plenty: " <> enumerate ejemplos
   <> subsection "The dual category" <> dualCat
   <> subsection "Natural transformations" <> natTrans
   <> subsection "Composition of functors and natural transformations" <> componerFunctors
   <> section "Commutative diagram and monad definition" <> objetivo





ejemplos :: LaTeXC l => l
ejemplos = item Nothing <> ej1 <> item Nothing <> ej2 <> item Nothing <> ej3 <> item Nothing <> ej4 <> "\n\n"
   where
      ej1 = " For any category " <> simC' <> " we have the " <> textit "identity functor " <> math ( ("Id"!:simC) <> " : " <> simC <> to <> simC) <> "."
      ej2 = " If " <> simC' <> " is a category and " <> math ((simCat "D") <> raw "\\subseteq" <> simC) <> " is a subcategory, one has the obvious " <> qts "inclusion functor " <>  math ( (simCat "D")<>(raw "\\hookrightarrow")<>simC )
            <> ". In particular, we have inclusion functors " <> math ( ((simCat "Vect") !: (mathbb "K"))<>(raw "\\hookrightarrow")<>(simCat "Grp")<>(raw "\\hookrightarrow")<>(simCat "Set") ) <> ". These functors are usually called the " <> qts "forgetful functors" <> "."
      ej3 = " The " <> textit "power set functor " <> math ( (simCat "Set") <> to <> (simCat "Set")) <> " which maps sets to their power sets; and maps functions " <> math ("f : X" <>to<>"Y") <> " to functions " <> math ( (raw "\\powerset")<>" (X)"<>to<>(raw "\\powerset")<>" (Y)" )
            <> " which take inputs " <> math ("U "<>(raw "\\subseteq")<>" X") <> " and return " <> (math "f(U)") <> ", the image of " <> math "U" <> " under " <> math "f" <> ", defined by " <> math ( ("f(U)"<>quad) =: (quad<>"{"<>quad<>"f(u) : "<>("u"`in_`"U")<>quad<>"}") )
      ej4 = " The fundamental group is a functor from " <> math ((simCat "Top")<>"*") <> ", the category of " <> textit "pointed topological spaces"
            <> footnote (" The objects of " <> math ((simCat "Top")<>"*") <> " are pairs " <> math "(X,x)" <> " consisting of a topological space " <> math "X" <> " and a point " <> math ("x" `in_` "X") <> ". A morphism " <> math ("f : (X,x)"<>to<>"(Y,y)") <> " in " <> math ((simCat "Top")<>"*")
                        <> " is a continuous map " <> math ("f : X"<>to<>"Y") <> " such that " <> math ("f(x)" =: "y") <> ". Composition of morphisms is defined as the composition of maps in the usual sense. " )
            <> ", to " <> math (simCat "Grp") <> ". More precisely, check that if " <> math ("f : (X,x)"<>to<>"(Y,y)") <> " is a morphism in " <> math ((simCat "Top")<>"*") <> ", then one can use f to define a group homomorphism " <> math ( (pi_!:"1")<>"(X,x)"<>to<>(pi_!:"1")<>"(Y,y)" ) <> ", and this yields a functor " <> math ((simCat "Top")<>"*"<>to<>(simCat "Grp")) <> "."



dualCat :: LaTeXC l => l
dualCat =
   textit (textbf "Def:") <> " Let " <> simC' <> " be any category. The " <> textbf "dual category " <> math (simC^:circ') <> " of " <> simC' <> " is informally speaking, obtained from " <> simC' <> " by " <> qts "reversing all the arrows" <> "."
   <> " This is:" <> equation_ ( (objCat simC^:circ')<>":="<>(objCat simC)<>quad<>";"<>quad<>(homXY (Just (simC^:circ')) "X" "Y")<>":="<>(homXY (Just simC) "Y" "X")<>quad<>"with"<>quad<>( (("f"^:circ')`circ`((raw "\\,")<>("g"^:circ'))) =: ((autoParens ("g"`circ`"f"))^:circ')) )
   <> theorem "prop" ( " The rule " <> math ("X" <> mapsto <> "X") <> ", " <> math ("f" <> mapsto <> ("f"^:circ')) <> " defines a contravariant functor " <>  math (simC <> to <> (simC^:circ')) )
   <> theorem "prop" ( " If " <> math (simC!:"1") <> " and " <> math (simC!:"2") <> " are two categories, a covariant functor " <> math (phiu<>" : "<>(simC!:"1")<>to<>(simC!:"2")) <> " can also be though of as a contravariant functor " <> math ( ((simC!:"1")^:circ') <> to <> (simC!:"2") )
                     <> ", or a contravariant functor " <> math (simC!:"1" <> to <> ((simC!:"2")^:circ')) <> " , or a covariant functor " <> math ( ((simC!:"1")^:circ')<>to<>((simC!:"2")^:circ') ) <> ". The same holds if we switch " <>  qts "covariant" <> " and " <>  qts "contravariant" <> " throughout the last sentence." )



natTrans :: LaTeXC l => l
natTrans =
   textit (textbf "Def:") <> " Let " <> math (simC!:"1") <> " and " <> math (simC!:"2") <> " be categories, and let " <> math ( phiu<>","<>psiu<>" : "<>(simC!:"1")<>to<>(simC!:"2") ) <> " be functors. A " <> textbf "morphism of functors" <> ", or a " <> textbf "natural transformation"
   <> ", " <> math (alpha<>":"<>phiu<>to<>psiu) <> ", is a rule which to every object " <> math ("X" `in_` (simC!:"1")) <> " assigns a morphism " <> math ( (alpha!:"X")<>" : "<>phiu<>"(X)"<>to<>psiu<>"(X)" ) <> " such that for any morphism " <> math ( "X"<>raw "\\xrightarrow{f}"<>"Y") <> " in " <> math (simC!:"1") <> ", the following diagram commutes: "
   <> diagrama1
   <> paragraph (textit "Def:") <> "We say that the collection " <> math ( (autoParens (alpha!:"X"))!:("X"`in_`simC) ) <> " is an " <> textbf "isomorphism (of functors)" <> " between " <> math phiu <> " and " <> math psiu
   <> " if each morphism " <> math (alpha!:"X") <> " is invertible. In this case the collection " <>  math (autoParens ((alpha!:"X")^:"-1")) <> " defines a morphism of functors from " <> math psiu <> " to " <> math phiu <> ". We call this collection " <> math (alpha^:"-1") <> "."



diagrama1 :: LaTeXC l => l
diagrama1 = raw " \\[\\begin{tikzcd}\\Phi (X) \\arrow{r}{\\Phi (f)} \\arrow[swap]{d}{\\alpha_X} & \\Phi (Y) \\arrow{d}{\\alpha_Y} \\\\ \\Psi (X)  \\arrow{r}{\\Psi (f)} & \\Psi (Y) \\end{tikzcd}\\] "



componerFunctors :: LaTeXC l => l
componerFunctors =
   textit (textbf "Def:") <> " Let " <> math (simC!:"1"<>","<>simC!:"2"<>","<>simC!:"3") <> " be categories and let " <> math ( phiu<>" : "<>simC!:"1"<>to<>simC!:"2"<>raw "\\,"<>","<>raw "\\,"<>psiu<>" : "<>simC!:"2 "<>to<>simC!:"3") <> " be functors. The " <> textbf "composed functor "
   <> math ( (psiu `circ` phiu) <> " : " <> simC!:"1" <> to <> simC!:"3" ) <> " assigns to each object " <> math ("X" `in_` (simC!:"1")) <> " the object " <> math ( psiDfiDX `in_` (simC!:"3") )
   <> "; and to each morphism " <> math "f" <> " of " <> math ( homXY (Just (simC!:"1")) "X" "Y" ) <> ", the morphism " <> math (  psiDfiDf `in_` (homXY (Just (simC!:"3")) psiDfiDX psiDfiDY)  )
   <> paragraph indent <> " Similarly, if " <> simC' <> " and " <> math (simCat "D") <> " are categories, " <> math ( phiu!:"1"<>","<>phiu!:"2"<>","<>phiu!:"3"<>" : "<>simC<>to<>(simCat "D") ) <> " are three functors, and " <> math ( alpha<>" : "<>phiu!:"1"<>to<>phiu!:"2"<>","<>raw "\\,"<>beta<>" : "<>phiu!:"2"<>to<>phiu!:"3" )
   <> " are natural transformations, invent the definition of the composition " <> math ( (beta`circ`alpha)<>" : "<>phiu!:"1"<>to<>phiu!:"3" )
   <> ". In fact, modulo some set-theoretical issues (which should be ignored at this point), one can define the category of functors " <> math ( (simCat "Funct")<>(autoParens(simC<>","<>simCat "D")) ) <> " whose objects are functors from " <> simC' <> " to " <> math (simCat "D") <> " and whose morphisms are natural transformations. "



objetivo :: LaTeXC l => l
objetivo =
   " The concept of monad is found deep within the theory of Categories, far beyond the point in which we are now. In fact, the full theory can be built without set theory, with its own beauties such as expressing algebraic identities as commutative diagrams. "
   <> " The concept of " <> textit "commutative diagram" <> " is itself basic for this approach, so it will be exposed now."
   <> "\n\n Also, i will include the definition of " <> textit "monad" <> " from " <> textit "Category Theory - Steve Awodey"
   <> paragraph (textit "Def:") <> " A diagram (such as the ones below) is " <> textbf "commutative" <> " when, for each pair of vertices " <> math "c" <> " and " <> math "c'" <> ", any two paths formed from directed edges leading from " <> math "c" <> " to " <> math "c'" <> " yield, by composition of labels, equal morphisms from " <> math "c" <> " to " <> math "c'" <> "."
   <> "\n\n A considerable part of the effectiveness of categorical methods rests on the fact that such diagrams in each situation vividly represent the actions of the arrows at hand."
   <> paragraph (textit "Def:") <> " A " <> textbf "monad" <> " on a category " <> simC' <> " consists of an endofunctor " <> math (" T :"<>simC<>to<>simC) <> ", and natural transformations " <> math ( eta<>":"<>("1"!:simC)<>to<>"T") <> ", and " <> math ( mu<>":"<>("T"^:"2")<>to<>"T")
   <> " satisfying the two commutative diagrams below, that is, " <> equation_ ( (mu`circ`(mu!:"T")) =: (mu`circ`"T"!:mu ) ) <> equation_ ( (mu`circ`(eta!:"T")) =: "1" =:(mu`circ`"T"!:eta) )
   <> " Note the formal analogy to the definition of a monoid. In fact, a monad is exactly the same thing as a " <> textit "monoidal monoid" <> " in the monoidal category " <> math (simC^:simC) <> " with composition as the monoidal product, " <> math ( "G"`otimes`"F" =: "G"`circ`"F" )
   <> paragraph " "
   <> diagrama2
   <> equation_ ( (mu`circ`(mu!:"T")) =: (mu`circ`("T"!:mu)) )
   <> paragraph " "
   <> diagrama3
   <> equation_ ( (mu`circ`(eta!:"T")) =: ("1"!:"T") =: (mu`circ`("T"!:eta)) )



diagrama2 :: LaTeXC l => l
diagrama2 = raw " \\[\\begin{tikzcd} T^{3} \\arrow{r}{T_\\mu} \\arrow[swap]{d}{\\mu_T} & T^{2} \\arrow{d}{\\mu} \\\\ T^{2}  \\arrow{r}{\\mu} & T \\end{tikzcd}\\] "



diagrama3 :: LaTeXC l => l
diagrama3 = raw " \\[\\begin{tikzcd} T \\arrow{r}{\\eta_T} \\arrow[swap]{dr}{1_T} & T^{2} \\arrow{d}{\\mu} & T \\arrow[swap]{l}{T_\\eta} \\arrow{dl}{1_T} \\\\  & T & \\end{tikzcd}\\] "




ejemploDiagrama :: LaTeXC l => l
ejemploDiagrama = raw " \\[\\begin{tikzcd}R \\arrow{r}{\\phi} \\arrow[swap]{d}{\\chi} & S \\arrow{d}{\\Psi} \\\\ R/I \\arrow{ur}{\\psi} \\arrow{r}{\\Phi} & T \\end{tikzcd}\\] "
