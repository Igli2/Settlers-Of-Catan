{-# LANGUAGE DeriveGeneric #-}

module ConfigLoader (
    GameConfig,
    MapConfig,
    SOCMap,
    createMapFromConfig,
    parseConfig,
    requiredVictoryPoints,
    minCardsForRobbery,
    mapFile
) where

import Data.Yaml
import GHC.Generics
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Exception (throw)

data TileConfig = TileConfig {
    x :: !Int,
    y :: !Int,
    tile :: !String
} deriving (Show, Generic)

instance FromJSON TileConfig

data MapConfig = MapConfig {
    width :: !Int,
    length :: !Int,
    fillTile :: !String,
    tiles :: ![TileConfig]
} deriving (Show, Generic)

instance FromJSON MapConfig

data GameConfig = GameConfig {
    requiredVictoryPoints :: !Int,
    minCardsForRobbery :: !Int,
    mapFile :: !FilePath
} deriving (Show, Generic)

instance FromJSON GameConfig

type SOCMap = Map.Map (Int, Int) String

--TODO
createMapFromConfig :: MapConfig -> SOCMap
createMapFromConfig (MapConfig w l fT tiles) = undefined
    where tileConfigToMapPair (TileConfig x y tile) = undefined

parseConfig :: FromJSON a => FilePath -> IO a
parseConfig path = do
    file <- decodeFileEither path

    case file of      
        Left err -> throw err
        Right cfg -> return cfg
