import System.IO
import Data.Char
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
data Figure = W | B | WL | BL | E
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
  show W = "w"
  show B = "b"
  show WL = "W"
  show BL = "B"
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
  putStrLn(show (mapa !! 0))
  putStrLn(show (mapa !! 1))
  putStrLn(show (mapa !! 2))
  putStrLn(show (mapa !! 3))
  putStrLn(show (mapa !! 4))
  putStrLn(show (mapa !! 5))
  putStrLn(show (mapa !! 6))
  putStrLn(show (mapa !! 7))


