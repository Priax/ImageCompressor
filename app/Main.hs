{-# LANGUAGE OverloadedStrings #-}
{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Main
-}
module Main (main) where

import System.Environment          (getArgs)
import System.Exit                 (exitWith, ExitCode(ExitFailure))
import System.IO                   (hPutStrLn, stderr, stdout)
import Control.Exception           (try, IOException)
import System.Random               (randomRIO)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Map.Strict            as Map
import qualified Data.Vector                as V

type Coordinate = (Int, Int)
type Color      = (Int, Int, Int)
type Point      = (Coordinate, Color)

data WeightedColor = WeightedColor
    { wcColor  :: !Color
    , wcCount  :: !Int
    , wcCoords :: ![Coordinate]
    } deriving (Show)

data ValidArgs = ValidArgs
    { vaColors   :: !Int
    , vaConv     :: !Double
    , vaFilename :: !FilePath
    }

data ColorAcc = ColorAcc !Int !Int !Int !Int   -- sr sg sb sc

data RawArgs = RawArgs
    { raNbColors :: Maybe Int
    , raConv     :: Maybe Double
    , raFilename :: Maybe FilePath
    }

emptyRawArgs :: RawArgs
emptyRawArgs = RawArgs Nothing Nothing Nothing

parseArgs :: [String] -> RawArgs -> RawArgs
parseArgs [] acc = acc
parseArgs ("-n" : n : xs) acc =
    case reads n of
        [(v, "")] | v > 0 -> parseArgs xs acc { raNbColors = Just v }
        _                  -> parseArgs xs acc
parseArgs ("-l" : l : xs) acc =
    case reads l of
        [(v, "")] | v >= 0 -> parseArgs xs acc { raConv = Just v }
        _                   -> parseArgs xs acc
parseArgs ("-f" : f : xs) acc = parseArgs xs acc { raFilename = Just f }
parseArgs (_ : xs) acc        = parseArgs xs acc

validateArgs :: RawArgs -> Maybe ValidArgs
validateArgs (RawArgs (Just n) (Just l) (Just f)) = Just (ValidArgs n l f)
validateArgs _                                     = Nothing

parseUInt :: BS.ByteString -> Maybe (Int, BS.ByteString)
parseUInt bs
    | BS.null bs                         = Nothing
    | BS.head bs < '0' || BS.head bs > '9' = Nothing
    | otherwise                          = Just (go 0 bs)
  where
    go !n s
        | BS.null s             = (n, s)
        | c >= '0' && c <= '9' = go (n * 10 + fromEnum c - 48) (BS.tail s)
        | otherwise             = (n, s)
      where c = BS.head s

parseCoord :: BS.ByteString -> Maybe (Coordinate, BS.ByteString)
parseCoord bs = do
    rest0 <- BS.stripPrefix "(" bs
    (x, rest1) <- parseUInt rest0
    rest2 <- BS.stripPrefix "," rest1
    (y, rest3) <- parseUInt rest2
    rest4 <- BS.stripPrefix ")" rest3
    return ((x, y), rest4)

parseColor :: BS.ByteString -> Maybe Color
parseColor bs = do
    rest0 <- BS.stripPrefix "(" bs
    (r, rest1) <- parseUInt rest0
    guard (r <= 255)
    rest2 <- BS.stripPrefix "," rest1
    (g, rest3) <- parseUInt rest2
    guard (g <= 255)
    rest4 <- BS.stripPrefix "," rest3
    (b, rest5) <- parseUInt rest4
    guard (b <= 255)
    _ <- BS.stripPrefix ")" rest5
    return (r, g, b)
  where
    guard True  = Just ()
    guard False = Nothing

parseLine :: BS.ByteString -> Maybe Point
parseLine bs = do
    (coord, rest) <- parseCoord bs
    rest' <- BS.stripPrefix " " rest
    color <- parseColor rest'
    return (coord, color)

parsePoints :: BS.ByteString -> Maybe [Point]
parsePoints = mapM parseLine . filter (not . BS.null) . BS.lines

buildColorGroups :: [Point] -> V.Vector WeightedColor
buildColorGroups = V.fromList . Map.elems . foldl insert Map.empty
  where
    insert m (coord, color) =
        Map.insertWith
            (\(WeightedColor c n cs) (WeightedColor _ m' ms) ->
                WeightedColor c (n + m') (cs ++ ms))
            color
            (WeightedColor color 1 [coord])
            m

colorDistSq :: Color -> Color -> Int
colorDistSq (r1, g1, b1) (r2, g2, b2) =
    let !dr = r1-r2; !dg = g1-g2; !db = b1-b2
    in dr*dr + dg*dg + db*db

colorDist :: Color -> Color -> Double
colorDist c1 c2 = sqrt (fromIntegral (colorDistSq c1 c2))

closestIdx :: Color -> V.Vector Color -> Int
closestIdx color centroids = V.minIndexBy (\a b -> compare (dist a) (dist b)) centroids
  where dist c = colorDistSq color c

recomputeCentroids :: V.Vector WeightedColor -> V.Vector Color -> V.Vector Color
recomputeCentroids wcolors centroids =
    let k    = V.length centroids
        zero = ColorAcc 0 0 0 0
        accs0 = Map.fromList [(i, zero) | i <- [0..k-1]]
        accs  = V.foldl' step accs0 wcolors
    in V.imap (finalize accs) centroids
  where
    step m (WeightedColor color !cnt _) =
        let idx = closestIdx color centroids
            (r, g, b) = color
        in Map.insertWith
               (\(ColorAcc sr sg sb sc) (ColorAcc sr' sg' sb' sc') ->
                   ColorAcc (sr+sr') (sg+sg') (sb+sb') (sc+sc'))
               idx
               (ColorAcc (r*cnt) (g*cnt) (b*cnt) cnt)
               m
    finalize m i old =
        case Map.lookup i m of
            Just (ColorAcc sr sg sb sc) | sc > 0 ->
                (sr `div` sc, sg `div` sc, sb `div` sc)
            _ -> old

hasConverged :: V.Vector Color -> V.Vector Color -> Double -> Bool
hasConverged old new threshold =
    V.all (< threshold) $ V.zipWith colorDist old new

initCentroids :: V.Vector WeightedColor -> Int -> IO (V.Vector Color)
initCentroids wcolors k
    | V.null wcolors = return V.empty
    | otherwise = do
        firstIdx <- randomRIO (0, V.length wcolors - 1)
        let first = wcColor (wcolors V.! firstIdx)
        kMeansPlusPlus wcolors (V.singleton first) (k - 1)

kMeansPlusPlus :: V.Vector WeightedColor -> V.Vector Color -> Int -> IO (V.Vector Color)
kMeansPlusPlus _       centroids 0         = return centroids
kMeansPlusPlus wcolors centroids remaining = do
    let weights = V.map weight wcolors
        total   = V.sum weights
    r <- randomRIO (0.0, total)
    let nextIdx = selectByWeight weights r
        next    = wcColor (wcolors V.! nextIdx)
    kMeansPlusPlus wcolors (V.snoc centroids next) (remaining - 1)
  where
    weight (WeightedColor color cnt _) =
        fromIntegral (V.minimum (V.map (colorDistSq color) centroids) * cnt) :: Double

selectByWeight :: V.Vector Double -> Double -> Int
selectByWeight ws target = go 0 target
  where
    n = V.length ws
    go i remaining
        | i >= n - 1        = n - 1
        | remaining <= ws V.! i = i
        | otherwise         = go (i+1) (remaining - ws V.! i)

kMeansLoop :: V.Vector WeightedColor -> Int -> Double -> V.Vector Color -> Int -> IO ()
kMeansLoop wcolors k threshold centroids itersLeft
    | itersLeft <= 0 = emit centroids
    | hasConverged centroids newCentroids threshold = emit newCentroids
    | otherwise =
        kMeansLoop wcolors k threshold newCentroids (itersLeft - 1)
  where
    newCentroids = recomputeCentroids wcolors centroids
    emit cs      = printClusters (assignClusters wcolors cs k) cs

kMeans :: [Point] -> Int -> Double -> IO ()
kMeans points k threshold = do
    let wcolors = buildColorGroups points
    let safeK   = min k (V.length wcolors)
    initial <- initCentroids wcolors safeK
    kMeansLoop wcolors safeK threshold initial maxIterations
  where
    maxIterations = 1000

assignClusters :: V.Vector WeightedColor -> V.Vector Color -> Int -> V.Vector [WeightedColor]
assignClusters wcolors centroids k =
    V.accum (flip (:)) (V.replicate k []) assignments
  where
    assignments =
        [ (closestIdx (wcColor wc) centroids, wc)
        | wc <- V.toList wcolors
        ]

printClusters :: V.Vector [WeightedColor] -> V.Vector Color -> IO ()
printClusters clusters centroids =
    BB.hPutBuilder stdout
    $ V.ifoldl' buildCluster mempty clusters
  where
    buildCluster acc i wcs =
        acc
        <> BB.string7 "--\n"
        <> colorBuilder (centroids V.! i)
        <> BB.char7 '\n'
        <> BB.string7 "-\n"
        <> foldMap pixelLines wcs

    pixelLines (WeightedColor color _ coords) =
        foldMap (\coord -> coordBuilder coord <> BB.char7 ' ' <> colorBuilder color <> BB.char7 '\n') coords

    colorBuilder (r, g, b) =
        BB.char7 '(' <> BB.intDec r
        <> BB.char7 ',' <> BB.intDec g
        <> BB.char7 ',' <> BB.intDec b
        <> BB.char7 ')'

    coordBuilder (x, y) =
        BB.char7 '(' <> BB.intDec x
        <> BB.char7 ',' <> BB.intDec y
        <> BB.char7 ')'

printUsage :: IO ()
printUsage =
    hPutStrLn stderr $ unlines
        [ "USAGE: ./imageCompressor -n N -l L -f F"
        , ""
        , "\tN\tnumber of colors in the final image"
        , "\tL\tconvergence limit"
        , "\tF\tpath to the file containing the colors of the pixels"
        ]

handleMain :: ValidArgs -> IO ()
handleMain va = do
    let n = vaColors va
        l = vaConv va
        f = vaFilename va
    result <- try (BS.readFile f) :: IO (Either IOException BS.ByteString)
    case result of
        Left  _     -> exitWith (ExitFailure 84)
        Right input ->
            case parsePoints input of
                Nothing     -> exitWith (ExitFailure 84)
                Just []     -> exitWith (ExitFailure 84)
                Just points -> kMeans points n l

main :: IO ()
main = do
    args <- getArgs
    case validateArgs (parseArgs args emptyRawArgs) of
        Nothing      -> printUsage >> exitWith (ExitFailure 84)
        Just valid   -> handleMain valid
