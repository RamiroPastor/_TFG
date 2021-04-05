{-# LANGUAGE OverloadedStrings #-}

module TFG_IOlibrary where



import Data.Text (pack)


import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath

import TFG_maybeMonad (bind)





-------- SIMBOLOS U OTRO CODIGO --------

c :: Char
c = toEnum 34

tronchoCodigo :: LaTeXC l => l
tronchoCodigo = verbatim (pack t) where
  t =
    "  data IOMode  = ReadMode   | WriteMode    \n"
    <> "               | AppendMode | ReadWriteMode \n \n"
    <> "  openFile     :: FilePath -> IOMode -> IO Handle \n"
    <> "  hClose       :: Handle -> IO () \n \n"
    <> "  hIsEOF       :: Handle -> IO Bool \n \n"
    <> "  hGetChar     :: Handle -> IO Char  \n"
    <> "  hGetLine     :: Handle -> IO String \n"
    <> "  hGetContents :: Handle -> IO String \n \n"
    <> "  getChar      :: IO Char \n"
    <> "  getLine      :: IO String \n"
    <> "  getContents  :: IO String \n \n"
    <> "  hPutChar     :: Handle -> Char -> IO () \n"
    <> "  hPutStr      :: Handle -> String -> IO () \n"
    <> "  hPutStrLn    :: Handle -> String -> IO () \n \n"
    <> "  putChar      :: Char -> IO () \n"
    <> "  putStr       :: String -> IO () \n"
    <> "  putStrLn     :: String -> IO () \n \n"
    <> "  readFile     :: FilePath -> IO String \n"
    <> "  writeFile    :: FilePath -> String -> IO () \n"

writeChar :: LaTeXC l => l
writeChar = verbatim (pack t) where
  t =
    "  writeChar :: FilePath -> Char -> IO ()  \n"
    <> "  writeChar fp c = \n"
    <> "      bracket \n"
    <> "        (openFile fp WriteMode) \n"
    <> "        hClose \n"
    <> "        (\\h -> hPutChar h c) \n"

fileReading :: LaTeXC l => l
fileReading = verbatim (pack t) where
  t =
    "module Main \n"
    <> "      where \n \n"
    <> "  import System.IO \n"
    <> "  import Control.Exception \n \n"
    <> "  main = doLoop \n \n"
    <> "  doLoop = do \n"
    <> "    putStrLn "<>[c]<>"Enter a command rFN wFN or q to quit:"<>[c]<>" \n"
    <> "    command <- getLine \n"
    <> "    case command of \n"
    <> "      'q':_ -> return () \n"
    <> "      'r':filename -> do putStrLn ("<>[c]<>"Reading "<>[c]<>" ++ filename) \n"
    <> "                         doRead filename \n"
    <> "                         doLoop \n"
    <> "      'w':filename -> do putStrLn ("<>[c]<>"Writing "<>[c]<>" ++ filename) \n"
    <> "                         doWrite filename \n"
    <> "                         doLoop \n"
    <> "      _            -> doLoop \n \n"
    <> "  doRead filename = \n"
    <> "      bracket (openFile filename ReadMode) hClose \n"
    <> "              (\\h -> do contents <- hGetContents h \n"
    <> "                        putStrLn "<>[c]<>"The first 100 chars:"<>[c]<>" \n"
    <> "                        putStrLn (take 100 contents)) \n \n"
    <> "  doWrite filename = do \n"
    <> "    putStrLn "<>[c]<>"Enter text to go into the file:"<>[c]<>" \n"
    <> "    contents <- getLine \n"
    <> "    bracket (openFile filename WriteMode) hClose \n"
    <> "            (\\h -> hPutStrLn h contents) \n"


----------------------------------------


anexo7 :: LaTeXC l => l
anexo7 =
  chapter "Appendix: The IO library"
  <> "Here, we'll explore the most commonly used elements of the System.IO module."
  <> tronchoCodigo
  <> paragraph "Note" <> fbox "FilePath" <> " is a " <> textit "type synonym" <> " for " <> fbox "String" 
  <> ". So, for instance, the " <> fbox "readFile" <> " function takes a " <> fbox "String" <> " (the file to read) and returns an action that, when run, produces the contents of that file."
  <> par <> "Most of the IO functions are self-explanatory. The " <> fbox "openFile" <> " and " <> fbox "hClose " <> " functions open and close a file, respectively. The " <> fbox "IOMode" 
  <> " argument determines the mode for opening the file. " <> fbox "hIsEOF" <> " tests for end-of file. " <> fbox "hGetChar" <> " and " <> fbox "hGetLine" <> " read a character or line (respectively) from a file. "
  <> fbox "hGetContents" <> " reads the entire file. The " <> fbox "getChar" <> ", " <> fbox "getLine" <> ", and " <> fbox "getContents" <> " variants read from standard input. " 
  <> fbox "hPutChar" <> " prints a character to a file; " <> fbox "hPutStr" <> " prints a string; and " <> fbox "hPutStrLn" <> " prints a string with a newline character at the end. The variants without the "
  <> fbox "h" <> " prefix work on standard output. The " <> fbox "readFile" <> " and " <> fbox "writeFile" <> " functions read and write an entire file without having to open it first."
  <> section "Bracket"
  <> "The " <> fbox "bracket" <> " function comes from the " <> fbox "Control.Exception" <> " module. It helps perform actions safely."
  <> verbatim ( pack "  bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c \n" )
  <> "Consider a function that opens a file, writes a character to it, and then closes the file. When writing such a function, one needs to be careful to ensure that, if there were an error at some point, the file is still successfully closed"
  <> ". The " <> fbox "bracket" <> " function makes this easy. It takes three arguments: The first is the action to perform at the beginning. The second is the action to perform at the end, regardless of whether there's an error or not. The third is the action to perform in the middle, which might result in an error. For instance, our character-writing function might look like:"
  <> writeChar
  <> "This will open the file, write the character, and then close the file. However, if writing the character fails, " <> fbox "hClose" <> " will still be executed, and the exception will be reraised afterwards. That way, you don't need to worry too much about catching the exceptions and about closing all of your handles."
  <> section "A file reading program" 
  <> "We can write a simple program that allows a user to read and write files. The interface is admittedly poor, and it does not catch all errors (such as reading a non-existent file). Nevertheless, it should give a fairly complete example of how to use IO. Enter the following code into " <> qts "FileRead.hs" <> ", and compile/run:"
  <> fileReading
  <> "What does this program do? First, it issues a short string of instructions and reads a command. It then performs a " <> textbf "case" <> " switch on the command and checks first to see if the first character is a `q'. If it is, it returns a value of unit type."
  <> paragraph "Note" <> "The " <> fbox "return" <> " function is a function that takes a value of type " <> fbox "a" <> " and returns an action of type " <> fbox "IO a" <> ". Thus, the type of " <> fbox "return ()" <> " is " <> fbox "IO ()" <> "."
  <> par <> "If the first character of the command wasn't a `q', the program checks to see if it was an `r' followed by some string that is bound to the variable " <> fbox "filename" <> ". It then tells you that it's reading the file, does the read and runs " <> fbox "doLoop" 
  <> " again. The check for `w' is nearly identical. Otherwise, it matches " <> fbox "_" <> ", the wildcard character, and loops to " <> fbox "doLoop" <> "."
  <> par <> "The " <> fbox "doRead" <> " function uses the " <> fbox "bracket" <> " function to make sure there are no problems reading the file. It opens a file in " <> fbox "ReadMode" <> ", reads its contents and prints the first 100 characters."
  <> par <> "The " <> fbox "doWrite" <> " function asks for some text, reads it from the keyboard, and then writes it to the specified file. "
  <> paragraph "Note" <> "Both " <> fbox "doRead" <> " and " <> fbox "doWrite" <> " could have been made simpler by using " <> fbox "readFile" <> " and " <> fbox "writeFile" <> ", but they were written in the extended fashion to show how the more complex functions are used."
  <> par <> "The program has one major problem: it will die if you try to read a file that doesn't already exist or if you specify some bad filename like " <> (verb "*bs^#_@") <> ". You may think that the calls to " <> fbox "bracket" <> " in " 
  <> fbox "doRead" <> " and " <> fbox "doWrite" <> " should take care of this, but they don't. They only catch exceptions within the main body, not within the startup or shutdown functions (" <> fbox "openFile" <> " and " <> fbox "hClose" <> ", in these cases). To make this completely reliable, we would need a way to catch exceptions raised by " <> fbox "openFile" <> "."










