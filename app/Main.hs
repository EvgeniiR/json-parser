module Main where

import Lib

main :: IO ()
main = do
    inp <- getLine

    if inp == "readfile" then
      do contents <- readFile "./app/json.txt"
         print (runParser jsonValue contents)
      else print (runParser jsonValue inp)
