{-# LANGUAGE DeriveGeneric #-}

module ConfigLoader (
    GameConfig(..),
    MapConfig(..),
    TileConfig,
    parseConfig,
) where

import Data.Yaml
import GHC.Generics
import Data.Maybe (fromJust)
import Control.Exception (throw)

type TileConfig = ((Int, Int), String)

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
