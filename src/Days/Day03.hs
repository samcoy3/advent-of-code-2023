module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import qualified Util.Parsers as P
import Data.Char (isDigit)
import qualified Util.Util as U
import Data.Tuple (swap)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = P.coordinateParser mapper 0
  where
    mapper c =
      Just $
        if
          | isDigit c -> Number (read . pure $ c)
          | c == '.' -> Dot
          | otherwise -> Symbol c

------------ TYPES ------------
data Entry = Number Int | Symbol Char | Dot
  deriving (Eq, Show)

isNumber :: Maybe Entry -> Bool
isNumber (Just (Number _)) = True
isNumber _ = False

isSymbol :: Maybe Entry -> Bool
isSymbol (Just (Symbol _)) = True
isSymbol _ = False

getNumber :: Maybe Entry -> Int
getNumber (Just (Number x)) = x
getNumber _ = error "Not a number!"

type Input = Map (Int, Int) Entry

type OutputA = Int

type OutputB = Int

------------ PART A ------------
symbolsInSurroundingCells :: Input -> (Int, Int) -> [(Int, Int)]
symbolsInSurroundingCells grid (x, y) =
  let surroundingCoords = (,) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]
   in filter (isSymbol . (grid Map.!?)) surroundingCoords

maybeParseNumberFromPosition :: Input -> (Int, Int) -> Maybe (Int, [(Int, Int)])
maybeParseNumberFromPosition grid (x, y) =
  if isNumber (grid Map.!? (x, y)) && not (isNumber (grid Map.!? (x - 1, y)))
    then Just $ readNumberAndSymbolsFromPosition grid (x, y)
    else Nothing

readNumberAndSymbolsFromPosition :: Input -> (Int, Int) -> (Int, [(Int, Int)])
readNumberAndSymbolsFromPosition = continueReadingPartNumber [] 0
  where
    continueReadingPartNumber adjacentSymbols accum grid (x, y) =
      let newNumber = accum * 10 + getNumber (grid Map.!? (x, y))
          adjacentSymbols' = nub $ adjacentSymbols ++ symbolsInSurroundingCells grid (x, y)
       in if not (isNumber (grid Map.!? (x + 1, y)))
            then (newNumber, adjacentSymbols')
            else
              continueReadingPartNumber
                adjacentSymbols'
                newNumber
                grid
                (x + 1, y)

partA :: Input -> OutputA
partA grid =
  sum
    . map fst
    . filter (not . null . snd)
    . mapMaybe (maybeParseNumberFromPosition grid)
    $ Map.keys grid

------------ PART B ------------
-- partB :: Input -> OutputB
partB grid =
  sum
    . Map.map product
    . Map.filterWithKey (\symbolCoords numbers -> length numbers == 2 && (grid Map.! symbolCoords) == Symbol '*')
    . foldr (\(symbolCoords, number) -> Map.insertWith (++) symbolCoords number) Map.empty
    . map (\(c, n) -> (c, [n]))
    . map swap
    . concatMap (\(number, symbols) -> map (number,) symbols)
    . mapMaybe (maybeParseNumberFromPosition grid)
    $ Map.keys grid
