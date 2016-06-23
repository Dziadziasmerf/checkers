module Board
( charRep
, stringToBoard
, printBoard
) where

import Def

charRep 'w' = W
charRep 'b' = B
charRep 'W' = WL
charRep 'B' = BL
charRep '.' = E

instance Show Figure where
  show W = "\9922"
  show B = "\9920" 
  show WL = "\9923"
  show BL = "\9921"
  show E = "."

stringToBoard :: String -> Board
  
stringToBoard stringBoard = map(map charRep)(lines stringBoard)

printBoard mapa = do
  putStrLn("0"++(show (mapa !! 0)))
  putStrLn("1"++(show (mapa !! 1)))
  putStrLn("2"++(show (mapa !! 2)))
  putStrLn("3"++(show (mapa !! 3)))
  putStrLn("4"++(show (mapa !! 4)))
  putStrLn("5"++(show (mapa !! 5)))
  putStrLn("6"++(show (mapa !! 6)))
  putStrLn("7"++(show (mapa !! 7)))
  putStrLn("  0|1|2|3|4|5|6|7")


