module Main where

import Data.Text.Table

main :: IO ()
main = do
  let ss = [ ["Header1", "Header2", "Header3"]
           , ["col1"   , "this is a longer col", "c3"]
           , ["x11"    , "x12"                 , "and finally this"]
           ]

  putStrLn $ tableToString ss [LeftJustify, RightJustify, CenterJustify]
