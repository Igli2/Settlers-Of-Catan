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
import qualified Data.Map as Map
import Control.Exception (throw)

-- data TileConfig = TileConfig {
    -- x :: !Int,
    -- y :: !Int,
    -- tile :: !String
-- } deriving (Show, Generic)
-- 
-- instance FromJSON TileConfig

type TileConfig = ((Int, Int), String)

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
    mapFile :: !FilePath,
    serverPort :: !Integer
} deriving (Show, Generic)

instance FromJSON GameConfig

parseConfig :: FromJSON a => FilePath -> IO a
parseConfig path = do
    file <- decodeFileEither path

    case file of      
        Left err -> throw err
        Right cfg -> return cfg
