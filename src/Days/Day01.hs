module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative (many, (<|>))
import Data.Either (rights)
import Data.Functor (($>))

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  many1 readDigit
    `sepBy1` (many letter *> endOfLine)
  where
    readDigit =
      (Right . read . pure <$> digit)
        <|> Left <$> wordDigit
        <|> letter *> readDigit

    -- Lookahead necessary here because words can overlap
    -- e.g. "eightwo"
    wordDigit =
      lookAhead
        ( ("one" $> 1)
            <|> ("two" $> 2)
            <|> ("three" $> 3)
            <|> ("four" $> 4)
            <|> ("five" $> 5)
            <|> ("six" $> 6)
            <|> ("seven" $> 7)
            <|> ("eight" $> 8)
            <|> ("nine" $> 9)
        )
        <* letter

------------ TYPES ------------
-- `Left` digits are letters; `Right` digits are numbers
type Input = [[Either Int Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map ((\l -> 10 * head l + last l) . rights)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map ((\l -> 10 * head l + last l) . map unwrapEither)
  where
    unwrapEither = \case
      Left i -> i
      Right i -> i
