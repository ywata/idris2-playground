module ReadWrite

import System
import System.Info
import System.File

abc : String
abc = "abc"
abcLF : String
abcLF = "abc\n"
abcCRLF : String
abcCRLF = "abc\r\n"

showInt : String -> String
showInt f = show . map (cast {to=Int}) . unpack $ f


writeFile : String -> IO()
writeFile f = do
         Right h <- openFile f ReadWriteTruncate
           | Left _ => exitFailure 
         Right _ <- fPutStr h abc
           | Left _ => exitFailure 
         Right _ <- fPutStrLn h abc
           | Left _ => exitFailure 
         Right _ <- fPutStr h abcLF
           | Left _ => exitFailure 
         Right _ <- fPutStr h abcCRLF
           | Left _ => exitFailure 
         _ <- closeFile h
         pure ()

readFile: String -> IO()
readFile f = do
         Right h <- openFile f Read
           | Left _ => exitFailure 
         showFile h
  where
    showFile : ?f -> IO ()
    showFile h = do
      if !(fEOF h)
        then pure ()
        else do
          Right l <- fGetLine h
            | Left _ => exitFailure
          putStrLn . showInt $ l
          showFile h

main : IO ()
main = do
  writeFile "abc.txt"
  readFile "abc.txt"



  
