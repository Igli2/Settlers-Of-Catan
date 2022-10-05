module GameData (
    CardType(..),
    PlaceableType(..),
    Player(..),
    GameState(..)
) where
import SOCMap (SOCMap)

import qualified Data.Map as Map

data CardType = DevelopmentCard String | ResourceCard String deriving (Show)
data PlaceableType = CornerPlaceable String | ConnectionPlaceable String deriving (Show)

data Player = Player {
    developmentCards :: Map.Map CardType Integer,
    resources :: Map.Map CardType Integer,
    placeables :: Map.Map PlaceableType Integer,
    victoryPoints :: Int
} deriving (Show)

data GameState = GameState {
    players :: [Player],
    map :: SOCMap
} deriving (Show)
