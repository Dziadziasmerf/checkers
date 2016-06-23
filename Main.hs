import System.IO
import Data.Char
import Board
import Move
import Def
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

main :: IO ()
main = do
  let mapa = stringToBoard initialBoardStr
  checkers White mapa

checkers :: Player -> Board -> IO ()
checkers player mapa = do
  printBoard mapa
  putStrLn $ "Current player " ++ show player
  move <- getLine
  let newMap = parse move mapa
  if or[mapa == newMap,not $ ownPoint player (read (take 5 move) :: (Int,Int)) mapa]
    then do 
      putStrLn "This move is not allowed. Please repeat:"
      checkers player mapa
    else do
      if and[(length (everyBeat nextPlayer mapa))==0,(length (everyMove nextPlayer mapa))==0]
        then do
          printBoard newMap
          putStrLn $ show player ++ " won!"
          return ()
        else checkers nextPlayer newMap
  where
    nextPlayer = changePlayer player

--bicie lub poruszanie w zależności od wprowadzonego tekstu
parse :: [Char] -> Board -> Board
parse str mapa
      | sign == '-' = moveFigure fig des mapa
      | sign == 'x' = beating fig des mapa
      | otherwise = mapa
  where
    fig = read (take 5 str) :: (Int,Int)
    des = read (drop 6 str) :: (Int,Int)
    sign = str !! 5

--sprawdzanie czy gracz chce poruszyć się swoim pionkiem
ownPoint :: Player -> (Int,Int) -> Board -> Bool 
ownPoint player (figR,figC) mapa
      |and[player==White,or[fig==W,fig==WL]] = True
      |and[player==Black,or[fig==B,fig==BL]] = True
      |otherwise = False
  where
     fig = ((mapa !!figR)!!figC)

--zmiana gracza
changePlayer :: Player -> Player
changePlayer player 
  |player == White = Black
  |otherwise = White
