{-# LANGUAGE TupleSections #-}
module GameData (
    CardType(..),
    PlaceableType(..),
    Player(..),
    TileMap,
    CornerMap,
    HexDirection(..),
    Position,
    PlayerName,
    getTilePosition,
    getTileCorners,
    getCornerNeighbours,
    createCornerMap
) where

import qualified Data.Map as Map

type PlayerName = String
type Position = (Integer, Integer)
type Color = (Int, Int, Int)
type TileMap = Map.Map Position String
type CornerMap = Map.Map Position Bool

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
getTilePosition pos []             = pos
getTilePosition (x, y) [NorthEast] = (if even y then x + 1 else x, y - 1)
getTilePosition (x, y) [East]      = (x + 1, y)
getTilePosition (x, y) [SouthEast] = (if even y then x + 1 else x, y + 1)
getTilePosition (x, y) [SouthWest] = (if odd y then x - 1 else x, y + 1)
getTilePosition (x, y) [West]      = (x - 1, y)
getTilePosition (x, y) [NorthWest] = (if odd y then x - 1 else x, y - 1)
getTilePosition pos (d:ds)         = getTilePosition (getTilePosition pos [d]) ds

getTileCorners :: Position -> [Position]
getTileCorners (x, y) = upperCorners ++ lowerCorners
    where upperCorners = map (\off -> (2 * x + off, y)) [-1, 0, 1]
          lowerCorners = map (\off -> (2 * x + off + lowerOff, y - 1)) [-1, 0, 1] 
          lowerOff = if odd y then -1 else 1

getCornerNeighbours :: Position -> [Position]
getCornerNeighbours (x, y) = verticalNeighbour : horizontalNeighbours
    where horizontalNeighbours = map (\off -> (x + off, y)) [-1, 1]
          verticalNeighbour = if even x then (x + 1, y + 1) else (x - 1, y - 1)

createCornerMap :: TileMap -> CornerMap
createCornerMap tM = Map.fromList . concat $ [map (, False) . getTileCorners $ pos | (pos, _) <- Map.toList tM]