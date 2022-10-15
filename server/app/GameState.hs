{-# LANGUAGE InstanceSigs #-}
module GameState (
    GameState(..),
    applyModifier,
    modifyPlayer,
    modifyTiles,
    modifyCorners,
    changeCardAmount,
    changeVictoryPoints,
    cardAmount,
    placeSettlement
) where

import GameData (PlayerName, Player(..), TileMap, CardType (ResourceCard), Position, PlaceableType (CornerPlaceable), CornerMap, getCornerNeighbours)
import qualified Data.Map as Map
import Control.Monad ((<=<))
import Data.Maybe (fromMaybe, isNothing, fromJust)

newtype Modifier a = M (a -> Either String a)

instance Semigroup (Modifier a) where
    (<>) :: Modifier a -> Modifier a -> Modifier a
    mA <> mB = M (applyModifier mB <=< applyModifier mA)

instance Monoid (Modifier a) where 
    mempty :: Modifier a
    mempty = M Right

data GameState = GameState {
    players :: Map.Map PlayerName Player,
    tileMap :: TileMap,
    cornerMap :: CornerMap
} deriving (Show)

applyModifier :: Modifier a -> a -> Either String a
applyModifier (M f) = f

modifyPlayer :: PlayerName -> Modifier Player -> Modifier GameState
modifyPlayer pName pMod = M (\g ->
        case Map.lookup pName . players $ g of
            Nothing -> Left $ "No player with the name '" ++ pName ++ "' found!"
            Just p -> applyModifier pMod p >>= (\p' -> Right g {players = Map.insert pName p' (players g)})
    )

modifyTiles :: Modifier TileMap -> Modifier GameState
modifyTiles mMod = M (\g -> applyModifier mMod (tileMap g) >>= (\m' -> Right g {tileMap = m'}))

modifyCorners :: Modifier CornerMap -> Modifier GameState
modifyCorners cMod = M (\g -> applyModifier cMod (cornerMap g) >>= (\c' -> Right g {cornerMap = c'}))

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

placeSettlement :: PlayerName -> Position -> Modifier GameState
placeSettlement pName pos = mconcat [modifyPlayer pName settlementCosts, placeCornerPlaceable (CornerPlaceable "settlement") pName pos]
    where settlementCosts = mconcat . map (\t -> changeCardAmount (ResourceCard t) (-1)) $ ["wool", "brick", "corn", "lumber"]

-- TODO: not every corner placeable needs distance of 2 from next; dont store corner as bool
placeCornerPlaceable :: PlaceableType -> PlayerName -> Position -> Modifier GameState
placeCornerPlaceable (CornerPlaceable pT) pName pos = M (\g ->
        if isPosValid (cornerMap g)
        then Right g {cornerMap = Map.insert pos True (cornerMap g)}
        else Left "It is not allowed to place sth. at this corner!"
    )
    where isPosValid cMap = not . any (fromMaybe False . flip Map.lookup cMap) $ getCornerNeighbours pos 
placeCornerPlaceable _ _ _ = undefined