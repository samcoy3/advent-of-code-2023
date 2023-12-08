module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = game `sepBy1` endOfLine
  where
    game = do
      "Game "
      gameId <- decimal
      ": "
      samples <- sample `sepBy1` "; "
      return Game {gameId = gameId, sampledCubes = samples}
    sample = foldr1 (<+>) <$> cubes `sepBy1` ", "
    cubes =
      ((decimal <* " red") >>= (\red -> return $ CubeCollection red 0 0))
        <|> ((decimal <* " green") >>= (\green -> return $ CubeCollection 0 green 0))
        <|> ((decimal <* " blue") >>= (\blue -> return $ CubeCollection 0 0 blue))

------------ TYPES ------------
data Game = Game {gameId :: Int, sampledCubes :: [CubeCollection]} deriving (Show)

data CubeCollection = CubeCollection {red :: Int, green :: Int, blue :: Int} deriving (Show)

-- Add two cube colections together
(<+>) :: CubeCollection -> CubeCollection -> CubeCollection
(<+>) c1 c2 =
  CubeCollection
    { red = red c1 + red c2,
      green = green c1 + green c2,
      blue = blue c1 + blue c2
    }

type Input = [Game]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
couldBeDrawnFrom :: CubeCollection -> CubeCollection -> Bool
couldBeDrawnFrom c1 c2 =
  (red c1 <= red c2)
    && (green c1 <= green c2)
    && (blue c1 <= blue c2)

partA :: Input -> OutputA
partA = sum . map gameId . filter gameIsPossible
  where
    threshold = CubeCollection {red = 12, green = 13, blue = 14}
    gameIsPossible = all (`couldBeDrawnFrom` threshold) . sampledCubes

------------ PART B ------------
-- Takes the colour-wise maximum of two cube collections
(<^>) :: CubeCollection -> CubeCollection -> CubeCollection
(<^>) c1 c2 =
  CubeCollection
    { red = red c1 `max` red c2,
      green = green c1 `max` green c2,
      blue = blue c1 `max` blue c2
    }

power :: CubeCollection -> Int
power CubeCollection {..} = red * green * blue

partB :: Input -> OutputB
partB = sum . map (power . foldr1 (<^>) . sampledCubes)
