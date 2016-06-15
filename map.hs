import System.IO
import Data.Char
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n.b......\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
data Figure = W | B | WL | BL | E deriving Eq
data Player = White | Black deriving (Eq, Show)
type Board = [[Figure]]

main :: IO ()
main = do
  let mapa = stringToBoard initialBoardStr
  checkers White mapa

checkers :: Player -> Board -> IO ()
checkers player mapa = do
  printBoard mapa
  putStrLn $ "Ruch gracza: " ++ show player
  move <- getLine
  let newMap = parse move mapa
  if mapa == newMap
    then do 
      putStrLn "Niedozwolony ruch. Powtorz"
      checkers player mapa
    else checkers (changePlayer player) newMap

parse :: [Char] -> Board -> Board
parse str mapa
      | sign == '-' = moveFigure fig des mapa
      | sign == 'x' = beating fig des mapa
      | otherwise = mapa
  where
    fig = read (take 5 str) :: (Int,Int)
    des = read (drop 6 str) :: (Int,Int)
    sign = str !! 5
  
changePlayer :: Player -> Player
changePlayer player 
  |player == White = Black
  |otherwise = White

charRep 'w' = W
charRep 'b' = B
charRep 'W' = WL
charRep 'B' = BL
charRep '.' = E

--figRep W = 'w'
--figRep B = 'b'
--figRep WL = 'W'
--figRep BL = 'B'
--figRep E = '.'

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


avaMoves :: Int -> Int -> Board -> [(Int,Int)]
avaMoves row col mapa =
  [(a,b)| a<-[0..7], b<-[0..7], and [abs(a-row)==1,abs(b-col)==1,((mapa !! a)!!b)==E, or[and[(mapa !! row)!!col==B,a>row],and[(mapa!! row)!! col==W,a<row]]]]

avaBeatings :: Int -> Int -> Board -> [(Int,Int)]
avaBeatings row col mapa = case ((mapa !! row) !! col) of 
     B -> [(a,b)| a<-[1..6], b<-[1..6], and[abs(a-row)==1,abs(b-col)==1,or[((mapa !!a)!!b)==W,((mapa !!a)!!b)==WL],((mapa !! (2*a-row))!!(2*b-col))==E]]
     W -> [(a,b)| a<-[1..6], b<-[1..6], and[abs(a-row)==1,abs(b-col)==1,or[((mapa !!a)!!b)==B,((mapa !!a)!!b)==BL],((mapa !! (2*a-row))!!(2*b-col))==E]]
     E -> []

everyBeat mapa =concat [avaBeatings a b mapa | a<-[0..7], b<-[0..7]]

moveFigure :: (Int,Int) -> (Int, Int) -> Board -> Board
moveFigure (figR, figC) (desR, desC) mapa 
      |elem (desR,desC) (avaMoves figR figC mapa) = changePos (desR, desC) fig $ deleteFig (figR, figC) mapa 
      |otherwise = mapa
  where
    fig = ((mapa !!figR)!!figC)


deleteFig :: (Int,Int) -> Board -> Board
deleteFig (figR, figC) mapa =
	take figR mapa ++ [(take figC (mapa !! figR) ++ [E] ++ drop (figC +1) (mapa !! figR))] ++ drop (figR+1) mapa

changePos :: (Int, Int) -> Figure -> Board -> Board
changePos (desR, desC) fig mapa =
	take desR mapa ++ [(take desC (mapa !! desR) ++ [fig] ++ drop (desC +1) (mapa !! desR))] ++ drop (desR+1) mapa

beating :: (Int,Int)->(Int,Int) -> Board -> Board
beating (figR, figC) (desR, desC) mapa 
      | elem (enemyR, enemyC) (avaBeatings figR figC mapa) = changePos (desR,desC) fig $ deleteFig (figR, figC) $ deleteFig (enemyR, enemyC) mapa
      | otherwise = mapa
  where 
    enemyR = div (figR+desR) 2
    enemyC = div (figC+desC) 2
    fig = ((mapa !!figR)!!figC)
--takeDown :: (Int, Int)
--if sprawdzanie czy dozwolony jak nie to mapa pozostaje bez zmian
--jeśli dozwolony to zmienia mape


writeToFile = do
  file <- openFile "map.txt" WriteMode
  putStrLn "Podaj mape:"
  map <- getLine
  hPutStrLn file map
  hClose file

readFromFile = do
  file <- openFile "map.txt" ReadMode
  mapFF <- hGetContents file
  hClose file
