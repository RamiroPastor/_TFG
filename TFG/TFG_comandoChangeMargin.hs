{-# LANGUAGE OverloadedStrings #-}


module TFG_comandoChangeMargin where


import Data.Text (pack)

import Text.LaTeX
import Text.LaTeX.Base.Class




cambiarMargenesCMD :: LaTeXC l => l
cambiarMargenesCMD = raw (pack t) where
  t =
    "\\newenvironment{changemargin}[2]{%              \n"
    <> "  \\begin{list}{}{%                           \n"
    <> "    \\setlength{\\topsep}{0pt}%                \n"
    <> "    \\setlength{\\leftmargin}{#1}%             \n"
    <> "    \\setlength{\\rightmargin}{#2}%            \n"
    <> "    \\setlength{\\listparindent}{\\parindent}%  \n"
    <> "    \\setlength{\\itemindent}{\\parindent}%     \n"
    <> "    \\setlength{\\parsep}{\\parskip}%           \n"
    <> "  }%                                         \n"
    <> "  \\item[]}{\\end{list}}                       \n"



usarCmdCambiarMargenes :: LaTeXC l => l -> l -> l -> l
usarCmdCambiarMargenes n1 n2 texto = auxf "\\begin{changemargin}{" <> n1 <> auxf "}{" <> n2 <> auxf "}" <> texto <> auxf "\\end{changemargin}"
  where auxf = raw . pack


margenesEstrechos :: LaTeXC l => l -> l
margenesEstrechos = usarCmdCambiarMargenes "-1cm" "-1cm"










{-

 ------------------------ ADJUNTO ARTICULO COMPLETO ------------------------

 Changing margins “on the fly”

 One of the surprises characteristic of TeX use is that you cannot change the width or height of the text within the document, simply by modifying the text size parameters; TeX can’t change the text width on the fly, and LaTeX only ever looks at text height when starting a new page. 

 So the simple rule is that the parameters should only be changed in the preamble of the document, i.e., before the \begin{document} statement (so before any typesetting has happened. 

 To adjust text width within a document we define an environment: 

 \newenvironment{changemargin}[2]{%
   \begin{list}{}{%
     \setlength{\topsep}{0pt}%
     \setlength{\leftmargin}{#1}%
     \setlength{\rightmargin}{#2}%
     \setlength{\listparindent}{\parindent}%
     \setlength{\itemindent}{\parindent}%
     \setlength{\parsep}{\parskip}%
   }%
   \item[]}{\end{list}}
-}
{-
 The environment takes two arguments, and will indent the left and right margins, respectively, by the parameters’ values. Negative values will cause the margins to be narrowed, so \begin{changemargin}{-1cm}{-1cm} narrows the left and right margins by 1 centimetre. 
 Given that TeX can’t do this, how does it work? - well, the environment (which is a close relation of the LaTeX quote environment) doesn’t change the text width as far as TeX is concerned: it merely moves text around inside the width that TeX believes in. 
 The changepage package provides ready-built commands to do the above; it includes provision for changing the shifts applied to your text according to whether you’re on an odd (recto) or an even (verso) page of a two-sided document. Changepage’s structure matches that of the memoir class. 
 The (earlier) package chngpage provides the same facilities, but it uses rather different syntax. Changepage’s structure matches that of the memoir class, and it should be used for any new work. 
 Changing the vertical dimensions of a page is more clumsy still: the LaTeX command \enlargethispage adjusts the size of the current page by the size of its argument. Common uses are 
 \enlargethispage{\baselineskip}
-} -} -}
{-
 to make the page one line longer, or 

 \enlargethispage{-\baselineskip}


 to make the page one line shorter. The process is (to an extent) simplified by the addlines package: its \addlines command takes as argument the number of lines to add to the page (rather than a length): the package documentation also provides a useful analysis of when the command may (or may not) be expected to work. 
 addlines.stymacros/latex/contrib/addlines (or browse the directory); catalogue entry changepage.stymacros/latex/contrib/changepage (or browse the directory); catalogue entry 
 This answer last edited: 2011-06-01


 This question on the Web: http://www.tex.ac.uk/cgi-bin/texfaq2html?label=chngmargonfly 



-} -}