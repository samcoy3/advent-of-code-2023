module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = game `sepBy1` endOfLine
  where
    game = do
      "Card"
      many1 space
      decimal
      ":"
      many1 space
      winningNumbers <- Set.fromList <$> decimal `sepBy1` many1 space
      " |"
      many1 space
      numbersYouHave <- Set.fromList <$> decimal `sepBy1` many1 space
      return (winningNumbers, numbersYouHave)

------------ TYPES ------------
type Input = [(Set Int, Set Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
cardScore :: Set Int -> Int
cardScore winners =
  if Set.null winners then 0 else 2 ^ (Set.size winners - 1)

partA :: Input -> OutputA
partA = sum . map (cardScore . uncurry Set.intersection)

------------ PART B ------------
partB :: Input -> OutputB
partB scratchcards = runScratchcards 1 (replicate (length scratchcards - 1) 0) scratchcards
  where
    runScratchcards numberOfCurrentCard futureCopyNumbers scratchcards =
      let nextScratchcard = head scratchcards
          nextScore = Set.size $ uncurry Set.intersection nextScratchcard
          newFutureCopyNumbers = zipWith (+) futureCopyNumbers (replicate nextScore numberOfCurrentCard ++ repeat 0)
       in if length scratchcards == 1
            then numberOfCurrentCard
            else
              numberOfCurrentCard
                + runScratchcards
                  (head newFutureCopyNumbers + 1)
                  (tail newFutureCopyNumbers)
                  (tail scratchcards)
