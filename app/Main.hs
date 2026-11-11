{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Main
-}
module Main (main) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStr, stderr, stdout)
import System.Random (initStdGen, mkStdGen)

import Args
import Image
import KMeans
import Output

main :: IO ()
main = getArgs >>= either argumentError run . parseArgs
  where
    argumentError err = failWith (renderArgError err ++ "\n\n" ++ usage)

run :: Options -> IO ()
run options = do
    contents <- try (BS.readFile (optFile options))
    case readImage contents of
        Left message -> failWith message
        Right image -> do
            gen <- maybe initStdGen (pure . mkStdGen) (optSeed options)
            let centroids =
                    kMeans
                        gen
                        (optColors options)
                        (optLimit options)
                        (palette image)
            BB.hPutBuilder stdout (renderClusters centroids image)
  where
    readImage :: Either IOException BS.ByteString -> Either String Image
    readImage contents = do
        input <- either (Left . show) Right contents
        either
            (Left . (optFile options ++) . (": " ++) . renderInputError)
            Right
            (parseImage input)

failWith :: String -> IO a
failWith message = hPutStr stderr (message ++ "\n") >> exitWith (ExitFailure 84)
