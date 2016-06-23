module Def(
Figure (..)
, Board (..)
, Player (..)) where

data Figure = W | B | WL | BL | E deriving Eq
data Player = White | Black deriving (Eq, Show)
type Board = [[Figure]]
