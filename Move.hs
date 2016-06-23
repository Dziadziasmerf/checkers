module Move
( avaMoves
, avaBeatings
, everyBeat
, everyMove
, moveFigure
, deleteFig
, changePos
, beating
) where

import Def

--wyswietla mozliwe ruchy dla danej figury
avaMoves :: Int -> Int -> Board -> [(Int,Int)]
avaMoves row col mapa =
  [(a,b)| a<-[0..7], b<-[0..7], and [abs(a-row)==1,abs(b-col)==1,((mapa !! a)!!b)==E, or[and[(mapa !! row)!!col==B,a>row],and[(mapa!! row)!! col==W,a<row]]]]

--wysiwtla mozliwe bicia dla danej figury
avaBeatings :: Int -> Int -> Board -> [(Int,Int)]
avaBeatings row col mapa = case ((mapa !! row) !! col) of 
     B -> [(a,b)| a<-[1..6], b<-[1..6], and[abs(a-row)==1,abs(b-col)==1,or[((mapa !!a)!!b)==W,((mapa !!a)!!b)==WL],((mapa !! (2*a-row))!!(2*b-col))==E]]
     W -> [(a,b)| a<-[1..6], b<-[1..6], and[abs(a-row)==1,abs(b-col)==1,or[((mapa !!a)!!b)==B,((mapa !!a)!!b)==BL],((mapa !! (2*a-row))!!(2*b-col))==E]]
     E -> []

--wyswietla wszystkie figury przeciwnika ktore dany gracz moze zbic
everyBeat player mapa = case player of
  White -> concat [avaBeatings a b mapa | a<-[0..7], b<-[0..7], or [((mapa !! a)!!b)==WL,((mapa !!a)!!b)==W]]
  Black -> concat [avaBeatings a b mapa | a<-[0..7], b<-[0..7], or [((mapa !! a)!!b)==BL,((mapa !!a)!!b)==B]]

--wyswietla wszystkie mozliwe ruchy w zaleznosci od gracza
everyMove player mapa = case player of
  White -> concat [avaMoves a b mapa | a<-[0..7], b<-[0..7], or [((mapa !!a)!!b)==WL,((mapa !!a)!!b)==W]]
  Black -> concat [avaMoves a b mapa | a<-[0..7], b<-[0..7], or [((mapa !!a)!!b)==BL,((mapa !!a)!!b)==B]]

--przesuwa figure
moveFigure :: (Int,Int) -> (Int, Int) -> Board -> Board
moveFigure (figR, figC) (desR, desC) mapa 
      |elem (desR,desC) (avaMoves figR figC mapa) = changePos (desR, desC) fig $ deleteFig (figR, figC) mapa 
      |otherwise = mapa
  where
    fig = ((mapa !!figR)!!figC)

--usuwa figure
deleteFig :: (Int,Int) -> Board -> Board
deleteFig (figR, figC) mapa =
	take figR mapa ++ [(take figC (mapa !! figR) ++ [E] ++ drop (figC +1) (mapa !! figR))] ++ drop (figR+1) mapa

--wstawia dana figure na podanej pozycji
changePos :: (Int, Int) -> Figure -> Board -> Board
changePos (desR, desC) fig mapa =
	take desR mapa ++ [(take desC (mapa !! desR) ++ [fig] ++ drop (desC +1) (mapa !! desR))] ++ drop (desR+1) mapa

--realizuje bicie
beating :: (Int,Int)->(Int,Int) -> Board -> Board
beating (figR, figC) (desR, desC) mapa 
      | elem (enemyR, enemyC) (avaBeatings figR figC mapa) = changePos (desR,desC) fig $ deleteFig (figR, figC) $ deleteFig (enemyR, enemyC) mapa
      | otherwise = mapa
  where 
    enemyR = div (figR+desR) 2
    enemyC = div (figC+desC) 2
    fig = ((mapa !!figR)!!figC)

