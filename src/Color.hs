{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Color
-}
module Color (
    Color,
    Centroid,
    distanceSq,
    toCentroid,
    fromCentroid,
    packColor,
    unpackColor,
    colorBuilder,
) where

import Data.ByteString.Builder qualified as BB

type Color = (Int, Int, Int)
type Centroid = (Double, Double, Double)

distanceSq :: (Num a) => (a, a, a) -> (a, a, a) -> a
distanceSq (r1, g1, b1) (r2, g2, b2) =
    square (r1 - r2) + square (g1 - g2) + square (b1 - b2)
  where
    square d = d * d

toCentroid :: Color -> Centroid
toCentroid (r, g, b) = (fromIntegral r, fromIntegral g, fromIntegral b)

-- | Rounds a centroid to the nearest color, for the output only.
fromCentroid :: Centroid -> Color
fromCentroid (r, g, b) = (component r, component g, component b)
  where
    component = max 0 . min 255 . round

-- | Packs a color into a single 24-bit key.
packColor :: Color -> Int
packColor (r, g, b) = (r * 256 + g) * 256 + b

unpackColor :: Int -> Color
unpackColor key = (key `quot` 65536, key `quot` 256 `rem` 256, key `rem` 256)

colorBuilder :: Color -> BB.Builder
colorBuilder (r, g, b) =
    BB.char7 '('
        <> BB.intDec r
        <> BB.char7 ','
        <> BB.intDec g
        <> BB.char7 ','
        <> BB.intDec b
        <> BB.char7 ')'
