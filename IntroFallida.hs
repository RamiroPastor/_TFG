{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Clock
import Data.Time.Calendar

import Text.LaTeX
import Data.Text (pack)




main :: IO ()
main = execLaTeXT tfg >>= renderFile "TFG.tex"





type LaTeXIO = LaTeXT IO

readIOString :: IO String -> LaTeXIO ()
readIOString s = lift s >>= verbatim . pack


fecha = getCurrentTime >>= return . show . toGregorian . utctDay
--   where f (yy,mm,dd) = 

fecha' = readIOString fecha 



intro :: LaTeX
intro = 
   documentclass [] report
   <> title "Monads in Haskell and Maths"
   <> author "Ramiro Pastor Martin"
 

tfg :: LaTeXIO ()
tfg = do
   lift (return intro)
   fecha'

   