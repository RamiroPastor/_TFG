{-# LANGUAGE OverloadedStrings #-}





module TFG_tablaMonadas where


import Data.Matrix
import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath



formatSym = scriptsize . fbox . math

-- verbbox :: LaTeXC l => l -> l
-- verbbox t = (raw "\\begin{verbbox}") <> t <> (raw "\\end{verbbox}")


-- tablaMonadas :: LaTeXC l => l
-- tablaMonadas = tabular Nothing [LeftColumn, VerticalLine, CenterColumn, VerticalLine, RightColumn] tabla 



-- tabla :: LaTeXC l => l
-- tabla =
  


tablaMonadas :: LaTeXC l => l
tablaMonadas = matrixTabular (fmap textbf ["Monad","Imperative Semantics","Found in Prelude"]) tabla


tabla :: Matrix LaTeX
tabla = matrix 7 3 asign 
  where
    asign (1,1) = fbox "Maybe"
    asign (2,1) = fbox "Error"
    asign (3,1) = fbox "State"
    asign (4,1) = fbox "IO"
    asign (5,1) = (fbox "[ ]") <> " (lists) "
    asign (6,1) = fbox "Reader"
    asign (7,1) = fbox "Writer"
    -----
    asign (1,2) = "Exception (anonymous)"
    asign (2,2) = "Exception (with error description)"
    asign (3,2) = "Global state"
    asign (4,2) = "Input/Output"
    asign (5,2) = "Nondeterminism"
    asign (6,2) = "Environment"
    asign (7,2) = "Logger"
    -----
    asign (1,3) = "Yes"
    asign (2,3) = "No"
    asign (3,3) = "No"
    asign (4,3) = "Yes"
    asign (5,3) = "Yes"
    asign (6,3) = "No"
    asign (7,3) = "No"





tablaTransformers :: LaTeXC l => l
tablaTransformers = matrixTabular (fmap textbf ["Base Monad","Transformer"] ++ [aux1,aux2]) tablaT
  where
    aux1 = parbox Nothing (Cm 2.5) ("Original Type \n\n" <> scriptsize ( qts "wrapped" <> " by base") )
    aux2 = parbox Nothing (Cm 3.5) ("Combined Type  \n\n" <> scriptsize ( qts "wrapped" <> " by transformer") )
    tablaT :: Matrix LaTeX 
    tablaT = matrix 4 4 asign
    -----
    asign (1,1) = "Writer"
    asign (2,1) = "Reader"
    asign (3,1) = "State"
    asign (4,1) = "Cont"
    -----
    asign (1,2) = "WriterT"
    asign (2,2) = "ReaderT"
    asign (3,2) = "StateT"
    asign (4,2) = "ContT"
    -----
    asign (1,3) = verb "(a, w)"
    asign (2,3) = verb "r -> a"
    asign (3,3) = verb "s -> (a, s)"
    asign (4,3) = verb "(a -> r) -> r"
    -----
    asign (1,4) = verb "m (a, w)"
    asign (2,4) = verb "r -> m a"
    asign (3,4) = verb "s -> m (a, s)"
    asign (4,4) = verb "(a -> m r) -> m r"



{-
tablaPeque :: LaTeXC l => l
tablaPeque = matrixTabular (fmap textbf ["do notation", "liftM"]) tablaP
  where
    tablaP :: Matrix LaTeX
    tablaP = matrix 1 2 asign
--    asign (1,1) = parbox Nothing (Cm 4) ( verb "do x <- monadicValue" <> newline <> verb "   return (f x)" ) 
--    asign (1,1) = parbox Nothing (Cm 4) ( verbatim "do x <- monadicValue  \n\n   return (f x)" )
    asign (1,1) = verb "do x <- monadicValue" <> verb "   return (f x)"
    asign (1,2) = verb "liftM f monadicValue"
-}


tablaPeque :: LaTeXC l => l
tablaPeque = verbatim $ pack t  where
  t =
    ".________________________________________________________________. \n"
    <> "|                               |                                | \n"
    <> "|         do notation           |            liftM               | \n"
    <> "|-------------------------------|--------------------------------| \n"
    <> "|     do x <- monadicValue      |     liftM f monadicValue       | \n"
    <> "|        return (f x)           |                                | \n"
    <> "|_______________________________|________________________________| \n"






tablaStateVsStateT :: LaTeXC l => l
tablaStateVsStateT = verbatim $ pack t  where
  t =
    ".___________________________________________________________________________________________. \n"
    <> "|                                         :                                                    | \n"
    <> "|                 State                   :                 StateT                             | \n"
    <> "|-----------------------------------------:----------------------------------------------------| \n"
    <> "|                                         :                                                    | \n"
    <> "| newtype State s a =                     : newtype StateT s m a =                             | \n"
    <> "|  State { runState :: (s -> (a,s)) }     :  StateT { runStateT :: (s -> m (a,s)) }            | \n"
    <> "|                                         :                                                    | \n"
    <> "| instance Monad (State s) where          : instance (Monad m) => Monad (StateT s m) where     | \n"
    <> "|  return a        = State $ \\s -> (a,s)  :  return a         = StateT $ \\s -> return (a,s)    | \n"
    <> "|  (State x) >>= f = State $ \\s ->        :  (StateT x) >>= f = StateT $ \\s -> do              | \n"
    <> "|   let (v,s') = x s                      :   (v,s') <- x s         -- get new value and state | \n"
    <> "|   in runState (f v) s'                  :   runStateT (f v) s'    -- pass them to f          | \n"  
    <> "|                                         :                                                    | \n"
    <> "|_________________________________________|____________________________________________________| \n"





tablaMonadVsApp :: LaTeXC l => l
tablaMonadVsApp = matrixTabular (fmap textbf ["Monadic","Applicative"] ++ [aux]) tablaAux
  where
    aux = parbox (Just Center) (Cm 5) ( "Module \n\n" <> scriptsize "(where to find the applicative version)" )
    tablaAux :: Matrix LaTeX 
    tablaAux = matrix 5 3 asign
    -----
    asign (1,1) = formatSym "(>>)"
    asign (2,1) = fbox "liftM2"
    asign (3,1) = fbox "mapM"
    asign (4,1) = fbox "sequence"
    asign (5,1) = fbox "forM_"
    -----
    asign (1,2) = formatSym "(*>)"
    asign (2,2) = fbox "liftA2"
    asign (3,2) = fbox "traverse"
    asign (4,2) = fbox "sequenceA"
    asign (5,2) = fbox "for_"
    -----
    asign (1,3) = "Prelude (GHC 7.10+); Control.Applicative "
    asign (2,3) = "Control.Applicative"
    asign (3,3) = "Prelude (GHC 7.10+); Data.Traversable"
    asign (4,3) = "Data.Traversable"
    asign (5,3) = "Data.Foldable"




tablaEqLaws :: LaTeXC l => l
tablaEqLaws = matrixTabular (fmap textbf ["Points-free style", "Do-block style"]) tablaAux
  where
    tablaAux :: Matrix LaTeX
    tablaAux = matrix 3 2 asign
    asign (1,1) = verb "  return x >>= f = f x  "
    asign (2,1) = verb "  m >>= return = m  "
    asign (3,1) = verb "  (m >>= f) >>= g = m >>= (\\x -> f x >>= g)  "
    -----
    asign (1,2) = verb "  do { v <- return x; f v } = do { f x }  "
    asign (2,2) = verb "  do { v <- m; return v } = do { m }  "
    asign (3,2) = minipage Nothing "6cm" ( verbatim $ pack $ "  do { y <- do { x <- m; f x }; g y}  \n= \n  do { x <- m; y <- f x; g y } " ) 


  

table_Sets :: LaTeXC l => l
table_Sets = matrixTabular (fmap textbf ["Function type", "Definition"]) tablaAux
  where
    tablaAux :: Matrix LaTeX
    tablaAux = matrix 3 2 asign
    asign (1,1) = math ( "P(f) :" <> mathcal "P" <> "(A)" <> to <> mathcal "P" <> "(B)" )
    asign (2,1) = math ( ("unit"!:"S") <> ": S" <> to <> mathcal "P" <> "(S)" )
    asign (3,1) = math ( ("join"!:"S") <> " : " <> mathcal "P" <> "(" <> mathcal "P" <> "(S))" <> to <> mathcal "P" <> "(S)" )
    -----
    asign (1,2) = math ( "(P(f))(S)" =: ("{f(a) : a" `in_` "S}") )
    asign (2,2) = math ( (("unit"!:"S") <> "(x)") =: "{x}" )
    asign (3,2) = math ( (("join"!:"S") <> "(L)") =: ( raw "\\bigcup" <> " L") ) 



table_Lists :: LaTeXC l => l
table_Lists = matrixTabular (fmap textbf ["Function type", "Definition"]) tablaAux
  where
    tablaAux :: Matrix LaTeX
    tablaAux = matrix 3 2 asign
    asign (1,1) = verb "  fmap f :: [A] -> [B]  "
    asign (2,1) = verb "  return :: T -> [T]  "
    asign (3,1) = verb "  join :: [[T]] -> [T]  "
    -----
    asign (1,2) = verb "  fmap f xs = [ f a | a <- xs ]   "
    asign (2,2) = verb "  return x = [x]  "
    asign (3,2) = verb "  join xs = concat xs  "
