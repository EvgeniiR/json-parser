module Main where

import Lib

main :: IO ()
main = do
    json <- getLine
    print (runParser jsonValue json)
