import System.IO
import Data.Char
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\n..w.w.w.\n...w.w.w\nw.w.w.w."
data Figure = W | B | WL | BL | E deriving Eq
type Board = [[Figure]]


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

stringToBoard :: String -> Board
  
stringToBoard stringBoard = map(map charRep)(lines stringBoard)

printBoard mapa = do
  putStrLn("8"++(show (mapa !! 0)))
  putStrLn("7"++(show (mapa !! 1)))
  putStrLn("6"++(show (mapa !! 2)))
  putStrLn("5"++(show (mapa !! 3)))
  putStrLn("4"++(show (mapa !! 4)))
  putStrLn("3"++(show (mapa !! 5)))
  putStrLn("2"++(show (mapa !! 6)))
  putStrLn("1"++(show (mapa !! 7)))
  putStrLn("  A|B|C|D|E|F|G|H")

avaMoves :: Int -> Int -> Board -> [(Int,Int)]
avaMoves row col mapa =
  [(a,b)| a<-[0..7], b<-[0..7], and [abs(a-row)==1,abs(b-col)==1,((mapa !! a)!!b)==E, or[and[(mapa !! row)!!col==B,a>row],and[(mapa!! row)!! col==W,a<row]]]]

moveFigure :: (Int,Int) -> (Int, Int) -> Board -> Board
--moveFigure (figR, figC) (desR, desC) mapa =
--if sprawdzanie czy dozwolony jak nie to mapa pozostaje bez zmian
--jeśli dozwolony to zmienia mape
