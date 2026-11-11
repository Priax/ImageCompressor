{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Image
-}
module Image (
    Coord,
    Image,
    imageCoords,
    imageColors,
    InputError (..),
    parseImage,
    renderInputError,
    pixelBuilder,
) where

import Control.Monad (guard, when)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M

import Color

type Coord = (Int, Int)

data Image = Image
    { imageCoords :: !(U.Vector Coord)
    , imageColors :: !(U.Vector Color)
    }

data InputError
    = MalformedLine !Int
    | ComponentOutOfRange !Int
    | DuplicateCoordinate !Coord
    | NoPixels
    deriving (Eq, Show)

renderInputError :: InputError -> String
renderInputError err = case err of
    MalformedLine n ->
        "line " ++ show n ++ ": expected a pixel as \"(x,y) (r,g,b)\""
    ComponentOutOfRange n ->
        "line " ++ show n ++ ": color components must be in 0..255"
    DuplicateCoordinate (x, y) ->
        "pixel (" ++ show x ++ "," ++ show y ++ ") is given twice"
    NoPixels -> "the file contains no pixel"

parseImage :: BS.ByteString -> Either InputError Image
parseImage input = do
    (coords, colors) <- U.unzip <$> readPixels input
    when (U.null coords) (Left NoPixels)
    maybe
        (Right (Image coords colors))
        (Left . DuplicateCoordinate)
        (findDuplicate coords)

readPixels :: BS.ByteString -> Either InputError (U.Vector (Coord, Color))
readPixels input = U.createT $ do
    out <- M.new (1 + BS.count '\n' input)
    let fill i n rest = case nextLine rest of
            Nothing -> pure (Right (M.take i out))
            Just (line, rest')
                | BS.null line -> fill i (n + 1) rest'
                | otherwise -> case parseLine n line of
                    Left err -> pure (Left err)
                    Right pixel ->
                        M.write out i pixel >> fill (i + 1) (n + 1) rest'
    fill 0 (1 :: Int) input

nextLine :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
nextLine input
    | BS.null input = Nothing
    | otherwise = Just (withoutCR line, BS.drop 1 rest)
  where
    (line, rest) = BS.span (/= '\n') input
    withoutCR l = fromMaybe l (BS.stripSuffix (BS.pack "\r") l)

parseLine :: Int -> BS.ByteString -> Either InputError (Coord, Color)
parseLine n line = case fields line of
    Nothing -> Left (MalformedLine n)
    Just (coord, r, g, b)
        | all inRange [r, g, b] -> Right (coord, (r, g, b))
        | otherwise -> Left (ComponentOutOfRange n)
  where
    inRange c = c <= 255

fields :: BS.ByteString -> Maybe (Coord, Int, Int, Int)
fields s0 = do
    (x, s1) <- natural =<< after "(" s0
    (y, s2) <- natural =<< after "," s1
    (r, s3) <- natural =<< after ") (" s2
    (g, s4) <- natural =<< after "," s3
    (b, s5) <- natural =<< after "," s4
    guard (s5 == BS.pack ")")
    pure ((x, y), r, g, b)
  where
    after prefix = BS.stripPrefix (BS.pack prefix)

natural :: BS.ByteString -> Maybe (Int, BS.ByteString)
natural s = BS.uncons s >>= \(c, _) -> guard (isDigit c) >> BS.readInt s

findDuplicate :: U.Vector Coord -> Maybe Coord
findDuplicate coords =
    (sorted U.!) <$> U.findIndex id (U.zipWith (==) sorted (U.tail sorted))
  where
    sorted = U.modify Intro.sort coords

pixelBuilder :: Coord -> Color -> BB.Builder
pixelBuilder (x, y) color =
    BB.char7 '('
        <> BB.intDec x
        <> BB.char7 ','
        <> BB.intDec y
        <> BB.string7 ") "
        <> colorBuilder color
        <> BB.char7 '\n'
