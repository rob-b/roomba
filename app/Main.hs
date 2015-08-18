{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Error
import Control.Monad
import Data.Attoparsec.Text
import Data.Maybe
import System.Exit

import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO


data Setup = Setup {
    gridSize       :: (Int, Int)
  , startPosition  :: (Int, Int)
  , trashPositions :: [(Int, Int)]
    } deriving Show


mkSetup :: [(Int, Int)] -> Maybe Setup
mkSetup (gs:sp:ts) = Just $ Setup gs sp ts
mkSetup _          = Nothing


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
    when (isNothing $ mkSetup digitLines) (TIO.putStrLn "Could not create initial values from input file" >> exitFailure)

    let initialValues = fromJust (mkSetup digitLines)

    -- let initialValues = maybe (TIO.putStrLn "Could not create initial values" >> exitFailure) (fromJust) (mkSetup digitLines)
    let roombaPositions = fmap (scanl (boundAddTuple $ gridSize initialValues) (startPosition initialValues)) (doLetters directions)
    let cleanedTrash = filter (\p -> maybe False (elem p) roombaPositions) (trashPositions initialValues)

    case last <$> roombaPositions of
      Just (x, y) -> putStrLn $ show x ++ " " ++ show y
      Nothing     -> print ("No positions detected"::String)
    print $ length cleanedTrash
