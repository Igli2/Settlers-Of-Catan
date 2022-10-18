{-# LANGUAGE DeriveGeneric #-}

module ConfigLoader (
    GameConfig(..),
    MapConfig(..),
    TileConfig,
    parseConfig,
    createMapFromConfig 
) where

import Data.Yaml
import GHC.Generics
import Data.Maybe (fromJust, fromMaybe)
import Control.Exception (throw)
import GameData (TileMap, Position, HexDirection(..), getTilePosition)
import qualified Data.Map as Map

type TileConfig = (Position, String)

data MapConfig = MapConfig {
    width    :: !Int,
    length   :: !Int,
    fillTile :: !String,
    tiles    :: ![TileConfig]
} deriving (Show, Generic)

instance FromJSON MapConfig

data GameConfig = GameConfig {
    requiredVictoryPoints :: !Int,
    minCardsForRobbery    :: !Int,
    mapFile               :: !FilePath,
    serverPort            :: !Integer,
    playerCount           :: !Int
} deriving (Show, Generic)

instance FromJSON GameConfig

parseConfig :: FromJSON a => FilePath -> IO a
parseConfig path = do
    file <- decodeFileEither path

    case file of      
        Left err -> throw err
        Right cfg -> return cfg

createMapFromConfig :: MapConfig -> TileMap
createMapFromConfig (MapConfig w l fT tiles) = Map.fromList [let pos = calculateTilePos (x, y) in (pos, getTileType pos) | y <- [-l..l], x <- [0..(2 * w - abs y)]]
    where getTileType pos = fromMaybe fT (lookup pos tiles)
          mostLeftTile = getTilePosition (0, 0) (replicate l SouthWest)

          addOffsetX xOff pos = getTilePosition pos $ replicate (abs xOff) (if xOff < 0 then West else East)
          addOffsetY yOff pos = getTilePosition pos $ replicate (abs yOff) (if yOff < 0 then NorthEast else SouthEast)
          calculateTilePos (xOff, yOff) = addOffsetX xOff . addOffsetY yOff $ mostLeftTile
