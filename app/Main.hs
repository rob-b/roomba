{-# LANGUAGE OverloadedStrings  #-}
module Main where
import System.Exit
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Attoparsec.Text
import Data.Foldable (toList)
import Control.Monad
import Control.Error


data Direction = North | South | East | West deriving (Show)
data Tile = Filled | Empty deriving (Eq, Show)
data Grid = Grid (S.Seq (S.Seq Tile))  deriving (Eq, Show)
data Setup = Setup {
    gridSize :: (Int, Int)
  , startPosition :: (Int, Int)
  , moves :: [(Int, Int)]
    }


gridToList :: Grid -> [[Tile]]
gridToList (Grid xs) = map toList (toList xs)


mkGrid :: Int -> Int -> Grid
mkGrid x y = Grid $ S.replicate y (S.replicate x Empty)


parseInt :: T.Text -> Either String Integer
parseInt = parseOnly (signed decimal)


toIntPair :: T.Text -> [Either String Integer]
toIntPair x = map parseInt (T.splitOn " " x)


show' :: Int -> T.Text
show' = T.pack . show


chunksOf1 :: String -> [T.Text]
chunksOf1 t = T.chunksOf 1 (T.pack t)


showGrid :: Grid -> T.Text
showGrid grid@(Grid _) = T.unlines $ map T.unwords $ map' asText (gridToList grid)
    where map' = map . map
          asText Empty  = "o"
          asText Filled = "x"


setPosition :: Grid -> Int -> Int -> Grid
setPosition (Grid xs) x y = let row = S.update x Filled (S.index xs y)
                                newGrid = S.update y row xs
                            in Grid newGrid


getInstructions :: IO [T.Text]
getInstructions = liftM T.lines (TIO.readFile "input.txt")


mkDirection :: T.Text -> Maybe Direction
mkDirection x = case x of
                  "N" -> Just North
                  "S" -> Just South
                  "W" -> Just West
                  "E" -> Just East
                  _   -> Nothing


-- | Parsing the input file contents
directionParser :: Parser String
directionParser = many1 $ satisfy isDir
        where
            -- inClass is slow in general more than likely ok for this simple
            -- case
            isDir = inClass "NSEW"


digitSpaceDigit :: Parser (Int, Int)
digitSpaceDigit = do
    a <- count 1 digit
    _ <- char ' '
    b <- count 1 digit
    return (read a, read b)


parseLine :: Parser a -> T.Text -> Either String a
parseLine p = parseOnly (p <* endOfInput)


main :: IO ()
main = do
    instructions <- getInstructions
    let digitLines = rights $ map (parseLine digitSpaceDigit) instructions
    let directions = rights $ map (parseLine directionParser) instructions

    when (length digitLines < 3) (TIO.putStrLn "Insufficient lines" >> exitFailure)
    when (length directions < 1) (TIO.putStrLn "No directions found" >> exitFailure)
    -- let directions' = concatMap chunksOf1 directions
    let grid = uncurry mkGrid (Prelude.head digitLines)
    TIO.putStrLn $ showGrid grid
