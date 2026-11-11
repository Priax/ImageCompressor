{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Output
-}
module Output (
    renderClusters,
) where

import Data.ByteString.Builder qualified as BB
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M

import Color
import Image
import KMeans (assign)

renderClusters :: U.Vector Centroid -> Image -> BB.Builder
renderClusters centroids image =
    foldMap cluster [0 .. U.length centroids - 1]
  where
    coords = imageCoords image
    colors = imageColors image
    assignment = assign centroids colors
    (starts, sizes, order) = groupByCluster (U.length centroids) assignment
    cluster j =
        BB.string7 "--\n"
            <> colorBuilder (fromCentroid (centroids U.! j))
            <> BB.string7 "\n-\n"
            <> U.foldr
                (\i rest -> pixel i <> rest)
                mempty
                (U.slice (starts U.! j) (sizes U.! j) order)
    pixel i = pixelBuilder (coords U.! i) (colors U.! i)

groupByCluster ::
    Int ->
    U.Vector Int ->
    (U.Vector Int, U.Vector Int, U.Vector Int)
groupByCluster k assignment = (starts, sizes, order)
  where
    sizes = U.accumulate (+) (U.replicate k 0) (U.map (\c -> (c, 1)) assignment)
    starts = U.prescanl' (+) 0 sizes
    order = U.create $ do
        sorted <- M.new (U.length assignment)
        next <- U.thaw starts
        U.iforM_ assignment $ \i c -> do
            position <- M.read next c
            M.write sorted position i
            M.write next c (position + 1)
        pure sorted
