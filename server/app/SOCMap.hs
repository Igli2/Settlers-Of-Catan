module SOCMap (
    HexDirection(..),
    SOCMap,
    createMapFromConfig
) where
    
import ConfigLoader (MapConfig(MapConfig), TileConfig) 
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data HexDirection = NorthEast | East | SouthEast | SouthWest | West | NorthWest

type SOCMap = Map.Map (Int, Int) String

createMapFromConfig :: MapConfig -> SOCMap
createMapFromConfig (MapConfig w l fT tiles) = Map.fromList [let pos = calculateTilePos (x, y) in (pos, getTileType pos) | y <- [-l..l], x <- [0..(2 * w - abs y)]]
    where getTileType pos = fromMaybe fT (lookup pos tiles)
          mostLeftTile = getTilePosition (0, 0) (replicate l SouthWest)

          addOffsetX xOff pos = getTilePosition pos $ replicate (abs xOff) (if xOff < 0 then West else East)
          addOffsetY yOff pos = getTilePosition pos $ replicate (abs yOff) (if yOff < 0 then NorthEast else SouthEast)
          calculateTilePos (xOff, yOff) = addOffsetX xOff . addOffsetY yOff $ mostLeftTile

getTilePosition :: (Int, Int) -> [HexDirection] -> (Int, Int)
getTilePosition pos [] = pos
getTilePosition (x, y) [NorthEast] = (if even y then x + 1 else x, y - 1)
getTilePosition (x, y) [East] = (x + 1, y)
getTilePosition (x, y) [SouthEast] = (if even y then x + 1 else x, y + 1)
getTilePosition (x, y) [SouthWest] = (if odd y then x - 1 else x, y + 1)
getTilePosition (x, y) [West] = (x - 1, y)
getTilePosition (x, y) [NorthWest] = (if odd y then x - 1 else x, y - 1)
getTilePosition pos (d:ds) = getTilePosition (getTilePosition pos [d]) ds
