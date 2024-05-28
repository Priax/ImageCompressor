{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Main
-}
module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Maybe (mapMaybe, fromJust)
-- import Data.Function (on)
import Data.List (elemIndex, transpose)
import System.IO
import Control.Exception (try, IOException)
import System.Random
-- import Data.Ord (comparing)

type Coordinate = (Int, Int)
type Color = (Int, Int, Int)
type Point = (Coordinate, Color)

data CommandArgs = CommandArgs { nbColors :: Maybe Int, conv :: Maybe Double, filename :: Maybe String }
--data Cluster = Cluster { moyenneRGB :: (Int, Int, Int), listPoint :: [Point], oldMoyenne :: (Int, Int, Int) } deriving (Show)

tryOpenFile :: FilePath -> IO (Either IOException Handle)
tryOpenFile fileName = try (openFile fileName ReadMode)

parseCoordinate :: String -> Maybe Coordinate
parseCoordinate str =
    case readMaybe str of
        Just (x, y) -> Just (x, y)
        _ -> Nothing

parseColor :: String -> Maybe Color
parseColor str =
    case readMaybe str of
        Just (r, g, b) ->
            if all (\c -> c >= 0 && c <= 255) [r, g, b]
                then Just (r, g, b)
                else Nothing
        _ -> Nothing

parsePoint :: String -> Maybe Point
parsePoint str =
    case words str of
        [coordStr, colorStr] ->
            case (parseCoordinate coordStr, parseColor colorStr) of
                (Just coord, Just color) -> Just (coord, color)
                _ -> Nothing
        _ -> Nothing

parsePoints :: String -> Maybe [Maybe Point]
parsePoints input =
    let parsed = map parsePoint (lines input)
    in if all (/= Nothing) parsed
        then Just parsed
        else Nothing

printUsage :: IO ()
printUsage = putStrLn $ "USAGE: ./imageCompressor -n N -l L -f F\n\n\t"
    ++ "N\tnumber of colors in the final image\n\t"
    ++ "L\tconvergence limit\n\t"
    ++ "F\tpath to the file containing the colors of the pixels"

parseArgs :: [String] -> CommandArgs -> CommandArgs
parseArgs [] args = args
parseArgs ("-n" : n : xs) args =
    case reads n of
        [(value, "")] | value > 0 ->
            parseArgs xs args { nbColors = Just value }
        _ -> parseArgs xs args
parseArgs ("-l" : l : xs) args =
    parseArgs xs args { conv = readMaybe l }
parseArgs ("-f" : f : xs) args =
    parseArgs xs args { filename = Just f }
parseArgs (_ : xs) args = parseArgs xs args

handleMain :: CommandArgs -> IO ()
handleMain CommandArgs { nbColors = Just n, conv = Just l, filename = Just f } = do
    fileExists <- tryOpenFile f
    case fileExists of
        Right _ -> do
            input <- readFile f
            case parsePoints input of
                Just pointsList -> kMeans (mapMaybe id pointsList) n l
                _ -> exitWith (ExitFailure 84)
        Left _ -> exitWith (ExitFailure 84)
handleMain _ = printUsage >> exitWith (ExitFailure 84)

generateRandomIndices :: Int -> Int -> IO [Int]
generateRandomIndices _ 0 = return []
generateRandomIndices listLength k
    | k >= listLength = return [0..(listLength - 1)]
    | otherwise = generateRandomIndices' listLength k []

generateRandomIndices' :: Int -> Int -> [Int] -> IO [Int]
generateRandomIndices' listLength k indices
    | length indices == k = return indices
    | otherwise = do
        randomIndex <- randomRIO (0, listLength - 1)
        if randomIndex `elem` indices
            then generateRandomIndices' listLength k indices
            else generateRandomIndices' listLength k (randomIndex : indices)

initializeCentroids :: [Point] -> Int -> IO [(Int, Int, Int)]
initializeCentroids pointsList k = do
    let numPoints = length pointsList
    randomIndices <- generateRandomIndices numPoints k
    let initialCentroids = map (pointsList !!) randomIndices
        centroids = map averageColor initialCentroids
    return centroids

averageColor :: Point -> (Int, Int, Int)
averageColor (_, color) = color

indexOfMinimum :: Ord a => [a] -> Int
indexOfMinimum xs = fromJust $ elemIndex (minimum xs) xs

indexOfMinimumOfEachList :: Ord a => [[a]] -> [Int]
indexOfMinimumOfEachList = map (indexOfMinimum)

groupPointsByCluster :: [Point] -> [Int] -> Int -> [[Point]]
groupPointsByCluster points indices k =
    map (getPointsWithIndex clusters) [0..k-1]
  where
    clusters = zip points indices

getPointsWithIndex :: [(Point, Int)] -> Int -> [Point]
getPointsWithIndex clusters index =
    map fst $ filter (\(_, i) -> i == index) clusters

updateCentroids :: [[Point]] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
updateCentroids clusters oldCentroids =
    zipWith updateCentroid clusters oldCentroids
  where
    updateCentroid :: [Point] -> (Int, Int, Int) -> (Int, Int, Int)
    updateCentroid points oldCentroid
      | null points = oldCentroid
      | otherwise =
        let (totalR, totalG, totalB, count) =
                foldr addColors (0, 0, 0, 0) points
        in (totalR `div` count, totalG `div` count, totalB `div` count)

addColors :: Point -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addColors ((_, (r, g, b))) (totalR, totalG, totalB, count) =
  (totalR + r, totalG + g, totalB + b, count + 1)

kMeans :: [Point] -> Int -> Double -> IO ()
kMeans pointsList k convergenceThreshold = do
    initialCentroids <- initializeCentroids pointsList k
    kMeansLoop pointsList k convergenceThreshold initialCentroids

kMeansLoop :: [Point] -> Int -> Double -> [(Int, Int, Int)] -> IO ()
kMeansLoop pointsList k convergenceThreshold centroids =
    let distances = calculateColorCentroidDistances pointsList centroids
        clusterAssignments = indexOfMinimumOfEachList $ transpose distances
        clusterized = groupPointsByCluster pointsList clusterAssignments k
        newCentroids = updateCentroids clusterized centroids
    in if hasConverged centroids newCentroids convergenceThreshold
       then printClusters clusterized newCentroids
       else kMeansLoop pointsList k convergenceThreshold newCentroids

printClusters :: [[Point]] -> [(Int, Int, Int)] -> IO ()
printClusters clusters centroids = mapM_ printCluster $ zip centroids clusters

printCluster :: ((Int, Int, Int), [Point]) -> IO ()
printCluster (centroid, points) =
    putStrLn "--" >>
    print centroid >>
    putStrLn "-" >>
    mapM_ printPoint' points

printPoint :: Point -> IO ()
printPoint ((x, y), (r, g, b)) =
    putStrLn $ "(" ++ show x ++ "," ++ show y ++ ") ("
    ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

printPoint' :: Point -> IO ()
printPoint' (coor, col) =
    putStrLn $ show coor ++ " " ++ show col

hasConverged :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Double -> Bool
hasConverged oldCentroids newCentroids threshold =
    all (< threshold) $ zipWith centroidDistance oldCentroids newCentroids

centroidDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
centroidDistance (r1, g1, b1) (r2, g2, b2) =
    sqrt $ fromIntegral ((r2 - r1) ^ (2 :: Int)
    + (g2 - g1) ^ (2 :: Int)
    + (b2 - b1) ^ (2 :: Int))

colorDistance :: Color -> Color -> Double
colorDistance (r1, g1, b1) (r2, g2, b2) =
  sqrt ((fromIntegral r1 - fromIntegral r2)**2 +
        (fromIntegral g1 - fromIntegral g2)**2 +
        (fromIntegral b1 - fromIntegral b2)**2)

calculateColorCentroidDistances :: [Point] -> [(Int, Int, Int)] -> [[Double]]
calculateColorCentroidDistances points centroids =
    map (\centroid ->
        map (\point -> colorDistance (snd point) centroid) points) centroids

main :: IO ()
main = do
    args <- getArgs
    let initialArgs = CommandArgs Nothing Nothing Nothing
        parsedArgs = parseArgs args initialArgs
    handleMain parsedArgs
