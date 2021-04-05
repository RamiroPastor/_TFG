{-# LANGUAGE OverloadedStrings #-}

module TFG_comandoReadCode where

import Text.LaTeX
import Text.LaTeX.Base.Class



readCode :: FilePath -> LaTeXT IO () 
readCode fp = lift (readFileTex fp) >>= verbatim