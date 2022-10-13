{-# LANGUAGE InstanceSigs #-}
module GameState (
    GameState(..),
    applyModifier,
    modifyPlayer,
    modifyMap,
    changeCardAmount,
    changeVictoryPoints,
    cardAmount
) where

import GameData (PlayerName, Player(..), TileMap, CardType (ResourceCard), Position, PlaceableType (CornerPlaceable))
import qualified Data.Map as Map
import Control.Monad ((<=<))
import Data.Maybe (fromMaybe)

newtype Modifier a = M (a -> Either String a)

instance Semigroup (Modifier a) where
    (<>) :: Modifier a -> Modifier a -> Modifier a
    mA <> mB = M (applyModifier mB <=< applyModifier mA)

instance Monoid (Modifier a) where 
    mempty :: Modifier a
    mempty = M Right

data GameState = GameState {
    players :: Map.Map PlayerName Player,
    tileMap :: TileMap
} deriving (Show)

applyModifier :: Modifier a -> a -> Either String a
applyModifier (M f) = f

modifyPlayer :: PlayerName -> Modifier Player -> Modifier GameState
modifyPlayer pName pMod = M (\g ->
        case Map.lookup pName . players $ g of
            Nothing -> Left $ "No player with the name '" ++ pName ++ " found!"
            Just p -> applyModifier pMod p >>= (\p' -> Right g {players = Map.insert pName p' (players g)})
    )

modifyMap :: Modifier TileMap -> Modifier GameState
modifyMap mMod = M (\g -> applyModifier mMod (tileMap g) >>= (\m' -> Right g {tileMap = m'}))

changeCardAmount :: CardType -> Integer -> Modifier Player
changeCardAmount cT delta = M (\p -> let currAmount = cardAmount cT p in 
            if currAmount + delta < 0
            then Left "Tried to remove more cards than the player has!"
            else Right p {playerCards = Map.insert cT (currAmount - delta) (playerCards p)}
        )

changeVictoryPoints :: Integer -> Modifier Player
changeVictoryPoints delta = M (\p -> let v' = victoryPoints p + delta in 
        if v' >= 0 
        then Right p {victoryPoints = v'}
        else Left "Tried to reduce the victory points of a player to a negative number!"
    )

cardAmount :: CardType -> Player -> Integer
cardAmount cT = fromMaybe 0 . Map.lookup cT . playerCards

-- placeSettlement :: PlayerName -> Position -> Modifier GameState
-- placeSettlement pName pos = mconcat [modifyPlayer pName settlementCosts, placePlaceable (CornerPlaceable "settlement") pName pos]
--     where settlementCosts = mconcat . map (\t -> changeCardAmount (ResourceCard t) (-1)) $ ["wool", "brick", "corn", "lumber"]

-- placePlaceable :: PlaceableType -> PlayerName -> Position -> Modifier GameState
-- placePlaceable = undefined --validation?