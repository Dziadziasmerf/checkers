import Data.Char
initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
data Figure = W | B | WL | BL | E  deriving Show
type Board = [[Figure]]


charRep 'w' = W
charRep 'b' = B
charRep 'W' = WL
charRep 'B' = BL
charRep '.' = E

figRep W = 'w'
figRep B = 'b'
figRep WL = 'W'
figRep BL = 'B'
figRep E = '.'

	
	
