module GameData (
    CardType(..),
    PlaceableType(..),
    Player(..),
    TileMap(..),
    HexDirection(..),
    Position,
    PlayerName,
    getTilePosition
) where

import qualified Data.Map as Map

type PlayerName = String
type Position = (Integer, Integer)
type Color = (Int, Int, Int)
type TileMap = Map.Map Position String

data CardType = DevelopmentCard String | ResourceCard String deriving (Show, Ord, Eq)
data PlaceableType = CornerPlaceable String | ConnectionPlaceable String deriving (Show, Ord, Eq)

data Player = Player {
    playerCards :: Map.Map CardType Integer,
    placeables :: Map.Map PlaceableType Integer,
    victoryPoints :: Integer,
    color :: Color
} deriving (Show)

data HexDirection = NorthEast | East | SouthEast | SouthWest | West | NorthWest

getTilePosition :: Position -> [HexDirection] -> Position
getTilePosition pos [] = pos
getTilePosition (x, y) [NorthEast] = (if even y then x + 1 else x, y - 1)
getTilePosition (x, y) [East] = (x + 1, y)
getTilePosition (x, y) [SouthEast] = (if even y then x + 1 else x, y + 1)
getTilePosition (x, y) [SouthWest] = (if odd y then x - 1 else x, y + 1)
getTilePosition (x, y) [West] = (x - 1, y)
getTilePosition (x, y) [NorthWest] = (if odd y then x - 1 else x, y - 1)
getTilePosition pos (d:ds) = getTilePosition (getTilePosition pos [d]) ds