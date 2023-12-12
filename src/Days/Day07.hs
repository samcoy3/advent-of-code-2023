module Days.Day07 (runDay) where

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
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Bifunctor (first)
import Data.Bits (Bits(xor))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = handWithBid `sepBy1` endOfLine
  where
    handWithBid = do
      cards <- count 5 card
      space
      bid <- decimal
      return (cards, bid)
    card =
      "A" $> Ace
        <|> "K" $> King
        <|> "Q" $> Queen
        <|> "J" $> Jack
        <|> "T" $> Ten
        <|> (Number . read . pure <$> satisfy isDigit)

------------ TYPES ------------
data Card = Number Int | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show)

newtype HandA = HandA {getHandA :: [Card]} deriving (Show)

instance Eq HandA where
  (HandA h1) == (HandA h2) = sort h1 == sort h2

instance Ord HandA where
  (HandA h1) <= (HandA h2) =
    scoreHand h1 < scoreHand h2
      || ( (scoreHand h1 == scoreHand h2) && h1 <= h2
         )

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

scoreHand :: [Card] -> HandType
scoreHand cards =
  if
    | 5 `elem` freqs -> FiveOfAKind
    | 4 `elem` freqs -> FourOfAKind
    | 3 `elem` freqs && 2 `elem` freqs -> FullHouse
    | 3 `elem` freqs -> ThreeOfAKind
    | sort (Map.elems freqs) == [1, 2, 2] -> TwoPair
    | 2 `elem` freqs -> OnePair
    | otherwise -> HighCard
  where
    freqs = U.freq cards

type Input = [([Card], Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . zipWith (\i (_, b) -> i * b) [1 ..] . sort . map (first HandA)

------------ PART B ------------
newtype CardB = CardB {getCard :: Card} deriving (Eq, Show)

instance Ord CardB where
  (CardB c1) <= (CardB c2) = value c1 <= value c2
    where
      value = \case
        Number x -> x
        Ten -> 10
        Queen -> 12
        King -> 13
        Ace -> 14
        Jack -> 0

newtype HandB = HandB {getHandB :: [CardB]} deriving (Show)

instance Eq HandB where
  h1 == h2 = getHandB h1 == getHandB h2

instance Ord HandB where
  h1@(HandB h1Cards) <= h2@(HandB h2Cards) =
    scoreHandB h1 < scoreHandB h2
      || ( (scoreHandB h1 == scoreHandB h2) && h1Cards <= h2Cards
         )

scoreHandB :: HandB -> HandType
scoreHandB =
  let possibleCards (CardB card) =
        if card == Jack
          then (Number <$> [2 .. 9]) ++ [Ten, Queen, King, Ace]
          else [card]
   in maximum . map scoreHand . mapM possibleCards . getHandB

partB :: Input -> OutputB
partB =
  sum
    . zipWith (\i (_, b) -> i * b) [1 ..]
    . sort
    . map (first (HandB . map CardB))
