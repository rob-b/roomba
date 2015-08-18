{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Error
import Control.Monad
import Data.Attoparsec.Text
import Data.List
import Data.Maybe
import Data.Foldable        (toList)
import System.Exit

import qualified Data.Sequence as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO


data Direction = North | South | East | West deriving (Show)
data Tile = Empty | Trash | Roomba deriving (Ord, Eq, Show)
data Grid = Grid (S.Seq (S.Seq Tile))  deriving (Eq, Show)

data Setup = Setup {
    gridSize       :: (Int, Int)
  , startPosition  :: (Int, Int)
  , trashPositions :: [(Int, Int)]
    } deriving Show

-- constructors for our data types
mkSetup :: [(Int, Int)] -> Setup
mkSetup (gs:sp:ts) = Setup gs sp ts


mkGrid :: (Int, Int) -> Grid
mkGrid (x, y) = Grid $ S.replicate y (S.replicate x Empty)


listToGrid :: [[Tile]] -> Grid
listToGrid ts =  Grid . S.fromList $ map S.fromList ts


gridToList :: Grid -> [[Tile]]
gridToList (Grid xs) = map toList (toList xs)


gridToListReverse :: Grid -> [[Tile]]
gridToListReverse (Grid xs) = map (reverse . toList) (reverse . toList $ xs)


showGrid :: Grid -> T.Text
showGrid grid@(Grid _) = T.unlines . map T.unwords $ map' asText (gridToListReverse grid)
    where map' = map . map
          asText :: Tile -> T.Text
          asText Empty  = "o"
          asText Roomba = "r"
          asText Trash  = "-"


setPosition :: Grid -> Tile -> (Int, Int) -> Grid
setPosition (Grid xs) tile (x,y) = let row = S.update x tile (S.index xs y)
                                       newGrid = S.update y row xs
                                    in Grid newGrid


rowWithRoomba :: [[Tile]] -> Maybe Int
rowWithRoomba ts = elemIndex Roomba targetRow
    where
        -- FIXME partial function that doesn't handle Nothing
        targetRow = ts !! fromJust (colWithRoomba ts)


colWithRoomba :: [[Tile]] -> Maybe Int
colWithRoomba = findIndex (elem Roomba)


coordsOfRoomba :: [[Tile]] -> Maybe (Int, Int)
coordsOfRoomba ts = y
    where
        z       = catMaybes [rowWithRoomba ts, colWithRoomba ts]
        y       = if length z == 2 then x z else Nothing
        x [a,b] = Just (a, b)


combine :: [[Tile]] -> [Tile]
combine xs = foldr (zipWith max) initialList xs
    where initialList = replicate (length $ head xs) Empty


getInstructions :: IO [T.Text]
getInstructions = liftM T.lines (TIO.readFile "input.txt")


-- | Parsing the input file contents
directionParser :: Parser String
directionParser = many1 $ satisfy isDir
        where
            -- inClass is slow in general but more than likely ok for this
            -- simple case
            isDir = inClass "NSEW"


digitSpaceDigit :: Parser (Int, Int)
digitSpaceDigit = do
    a <- count 1 digit
    _ <- char ' '
    b <- count 1 digit
    return (read a, read b)


parseLine :: Parser a -> T.Text -> Either String a
parseLine p = parseOnly (p <* endOfInput)


letterToDirection :: Char -> (Int, Int)
letterToDirection l
    | l ==  'N' =  (0, 1)
    | l ==  'S' =  (0, -1)
    | l ==  'E' =  (1, 0)
    | l ==  'W' =  (-1, 0)
    | otherwise =  (0, 0)


doLetters :: Maybe String -> Maybe [(Int, Int)]
doLetters = fmap (\ l -> [letterToDirection x | x <- l])


move :: Grid -> (Int, Int) -> Grid
move grid direction = maybe grid newGrid newPosition
    where
        newGrid (a, b) = do

            -- since this is only called when newPosition is a Just value we
            -- can assume that fromJust is safe to use
            let grid' = setPosition grid Empty (fromJust roombaLoc)
            setPosition grid' Roomba (a, b)

        roombaLoc = coordsOfRoomba $ gridToList grid
        newPosition = fmap (addTuple direction) roombaLoc


addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (a, b) (c, d) = (a + c, b + d)


boundAddTuple :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
boundAddTuple (xUpper, yUpper) old new = (min xUpper newX, min yUpper newY)
    where
        added = addTuple old new
        newX = fst added
        newY = snd added


main :: IO ()
main = do
    instructions <- getInstructions
    let digitLines = rights $ map (parseLine digitSpaceDigit) instructions
    let directions = headMay . rights $ map (parseLine directionParser) instructions

    -- exit early if the digits and directions don't match our expectations
    when (length digitLines < 3) (TIO.putStrLn "Insufficient lines" >> exitFailure)
    when (isNothing directions) (TIO.putStrLn "No directions found" >> exitFailure)

    let initialValues = mkSetup digitLines
    let roombaPositions = fmap (scanl (boundAddTuple $ gridSize initialValues) (startPosition initialValues)) (doLetters directions)
    let cleanedTrash = filter (\p -> maybe False (elem p) roombaPositions) (trashPositions initialValues)

    case last <$> roombaPositions of
      Just (x, y) -> putStrLn $ show x ++ " " ++ show y
      Nothing     -> print ("No positions detected"::String)
    print $ length cleanedTrash

    -- initialise a blank grid to the given dimensions
    let grid = mkGrid (gridSize initialValues)

    -- place the roomba in its starting position
    let grid' = setPosition grid Roomba (startPosition initialValues)

    -- create n grids to represent the n trash piles and cons them to the roomba grid
    let setTrashPosition = setPosition grid Trash
    let allGrids = grid' : map setTrashPosition (trashPositions initialValues)
    let tiles = map gridToList allGrids

    -- The transpose function transposes the rows and columns of its argument. For example,
    -- transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
    let combined = listToGrid $ map combine (transpose tiles)
    TIO.putStrLn $ showGrid combined
