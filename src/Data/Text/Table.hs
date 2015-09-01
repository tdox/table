module Data.Text.Table where

import Data.List (transpose)


data Justification = LeftJustify | CenterJustify | RightJustify


padString :: String -> Int -> Justification -> String
padString s fieldWidth j = lft ++ s ++ rght
  where
    diff = fieldWidth - length s
    
    (lft, rght) = case j of
      LeftJustify   -> (" ", makeSpaces (diff + 1))
      CenterJustify -> (makeSpaces (nLeft + 1), makeSpaces (nRight + 1))
      RightJustify  -> (makeSpaces (diff + 1), " ")
      
    (nLeft, nRight) = splitDiff diff
      
    splitDiff :: Int -> (Int, Int)  
    splitDiff n = if even n then (half, half) else (half + 1, half)
      where half = n `div` 2

    makeSpaces :: Int -> String
    makeSpaces n = concat $ replicate n " "


columnWidths :: [[String]] -> [Int]
columnWidths table
  | not $ rowsHaveSameLength table = error "columnWidths: unequal row length"
  | otherwise = map maxColWidth cols
    where
      cols = transpose table
      maxColWidth :: [String] -> Int
      maxColWidth cs = maximum $ map length cs 

      
rowsHaveSameLength :: [[a]] -> Bool
rowsHaveSameLength [] = True
rowsHaveSameLength (row:rows) = all (\r -> length row == length r) rows

-- [ ] To Do: change String to Text

tableToString :: [[String]] -> [Justification] -> String
tableToString table js
  | length (head table) /= length js
       = error "tableToString: table and js not same length"
         
  | otherwise = concat rows
  where
    colWidths = columnWidths table    :: [Int]
    rows = map formRow table :: [String]
    
    formRow :: [String] -> String
    formRow els =
      concat (zipWith3 padString els colWidths js) ++ "\n"
