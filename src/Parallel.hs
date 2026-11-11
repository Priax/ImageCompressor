{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Parallel
-}
module Parallel (
    parallelChunks,
) where

import Data.Vector.Unboxed qualified as U
import GHC.Conc (numCapabilities, par, pseq)

parallelChunks :: (U.Unbox a) => (U.Vector a -> b) -> U.Vector a -> [b]
parallelChunks f vector = sparkAll (map f (chunks vector))
  where
    sparkAll results = foldr par () results `pseq` results

chunks :: (U.Unbox a) => U.Vector a -> [U.Vector a]
chunks vector =
    [U.slice start (min size (n - start)) vector | start <- [0, size .. n - 1]]
  where
    n = U.length vector
    size = max minChunk ((n + numCapabilities - 1) `div` numCapabilities)

minChunk :: Int
minChunk = 4096
