{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- KMeans
-}
module KMeans (
    Palette,
    palette,
    kMeans,
    assign,
) where

import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Unboxed qualified as U
import System.Random (StdGen, uniformR)

import Color
import Image
import Parallel

data Palette = Palette !(U.Vector Color) !(U.Vector Int)

palette :: Image -> Palette
palette image = Palette (U.map (unpackColor . (keys U.!)) starts) counts
  where
    keys = U.modify Intro.sort (U.map packColor (imageColors image))
    starts = U.findIndices id (U.imap startsRun keys)
    startsRun i key = i == 0 || keys U.! (i - 1) /= key
    counts = U.zipWith (-) (U.snoc (U.tail starts) (U.length keys)) starts

maxIterations :: Int
maxIterations = 1000

kMeans :: StdGen -> Int -> Double -> Palette -> U.Vector Centroid
kMeans gen k limit pal = iterateFrom maxIterations (seed gen k pal)
  where
    points = U.map toCentroid colors
    Palette colors weights = pal
    iterateFrom remaining centroids
        | remaining <= 0 || converged centroids next = next
        | otherwise = iterateFrom (remaining - 1) next
      where
        next = step points colors weights centroids
    converged old new = old == new || U.and (U.zipWith closeEnough old new)
    closeEnough a b = distanceSq a b < limit * limit

seed :: StdGen -> Int -> Palette -> U.Vector Centroid
seed gen0 k (Palette colors weights) =
    U.fromListN k (map toCentroid (draw gen1 (k - 1) first (distancesTo first)))
  where
    (firstIndex, gen1) = uniformR (0, U.length colors - 1) gen0
    first = colors U.! firstIndex
    distancesTo c = U.map (distanceSq c) colors
    draw gen remaining chosen nearestSq = chosen : more
      where
        scores = U.zipWith (*) nearestSq weights
        total = U.sum scores
        (target, gen') = uniformR (0, total - 1) gen
        next = colors U.! pick target scores
        more
            | remaining <= 0 || total <= 0 = []
            | otherwise =
                draw
                    gen'
                    (remaining - 1)
                    next
                    (U.zipWith min nearestSq (distancesTo next))

pick :: Int -> U.Vector Int -> Int
pick target scores = go 0 0
  where
    lastIndex = U.length scores - 1
    go i acc
        | i >= lastIndex || acc' > target = i
        | otherwise = go (i + 1) acc'
      where
        acc' = acc + scores U.! i

type ColorSum = (Color, Int)

addSums :: ColorSum -> ColorSum -> ColorSum
addSums ((r, g, b), n) ((r', g', b'), n') = ((r + r', g + g', b + b'), n + n')

step ::
    U.Vector Centroid ->
    U.Vector Color ->
    U.Vector Int ->
    U.Vector Centroid ->
    U.Vector Centroid
step points colors weights centroids =
    U.zipWith mean centroids (foldr (U.zipWith addSums) zeros partialSums)
  where
    zeros = U.replicate (U.length centroids) ((0, 0, 0), 0)
    partialSums = parallelChunks sumChunk (U.zip3 points colors weights)
    sumChunk = U.accumulate addSums zeros . U.map contribution
    contribution (point, (r, g, b), n) =
        (nearest centroids point, ((r * n, g * n, b * n), n))

mean :: Centroid -> ColorSum -> Centroid
mean old ((r, g, b), n)
    | n > 0 = (r // n, g // n, b // n)
    | otherwise = old
  where
    s // d = fromIntegral s / fromIntegral d

assign :: U.Vector Centroid -> U.Vector Color -> U.Vector Int
assign centroids =
    U.concat . parallelChunks (U.map (nearest centroids . toCentroid))

nearest :: U.Vector Centroid -> Centroid -> Int
nearest centroids point = go 0 0 (1 / 0)
  where
    k = U.length centroids
    go i best bestDistance
        | i >= k = best
        | d < bestDistance = go (i + 1) i d
        | otherwise = go (i + 1) best bestDistance
      where
        d = distanceSq point (centroids U.! i)
