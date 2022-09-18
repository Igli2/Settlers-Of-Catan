{-# LANGUAGE DeriveGeneric #-}

module ConfigLoader (
    TileConfig,
    MapConfig
) where

import Data.Yaml
import GHC.Generics
import Data.Maybe (fromJust)

data TileConfig = TileConfig {
    x :: !Int,
    y :: !Int,
    tile :: !String
} deriving (Show, Generic)

data MapConfig = MapConfig {
    width :: !Int,
    length :: !Int,
    fillTile :: !String,
    tiles :: ![TileConfig]
} deriving (Show, Generic)

data GameConfig = GameConfig {
    requiredVictoryPoints :: !Int,
    minCardsForRobbery :: !Int,
    boardFile :: !FilePath
} deriving (Show, Generic)

instance FromJSON MapConfig
instance FromJSON TileConfig
instance FromJSON GameConfig