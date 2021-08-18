module FGetLine

import System
import System.Info
import System.File

showHex : String -> String
showHex f = show . map (cast {to=Int}) . unpack $ f


showFile: String -> IO()
showFile f = do
         Right h <- openFile f Read
           | Left _ => exitFailure 
         Right l1 <- fGetLine h
           | Left _ => exitFailure 
         putStrLn . showHex $ l1
         Right l2 <- fGetLine h
           | Left _ => exitFailure 
         putStrLn . showHex $ l2


main : IO ()
main = do
  showFile "LF.txt"
  showFile "CRLF.txt"


  
