{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Args
-}
module Args (
    Options (..),
    ArgError (..),
    parseArgs,
    renderArgError,
    usage,
) where

import Data.List (group, sort)
import Text.Read (readMaybe)

data Options = Options
    { optColors :: !Int
    , optLimit :: !Double
    , optFile :: !FilePath
    , optSeed :: !(Maybe Int)
    }

data ArgError
    = UnknownArgument String
    | MissingValue String
    | MissingOption String
    | RepeatedOption String
    | InvalidValue String String
    deriving (Eq, Show)

parseArgs :: [String] -> Either ArgError Options
parseArgs args = do
    pairs <- optionPairs args
    case [flag | flag : _ : _ <- group (sort (map fst pairs))] of
        flag : _ -> Left (RepeatedOption flag)
        [] ->
            Options
                <$> required "-n" positiveInt pairs
                <*> required "-l" limit pairs
                <*> required "-f" (const Right) pairs
                <*> traverse (anyInt "-s") (lookup "-s" pairs)
  where
    required flag parser pairs =
        maybe (Left (MissingOption flag)) (parser flag) (lookup flag pairs)

optionPairs :: [String] -> Either ArgError [(String, String)]
optionPairs [] = Right []
optionPairs (flag : _)
    | flag `notElem` flags = Left (UnknownArgument flag)
optionPairs (flag : value : rest)
    | value `notElem` flags = ((flag, value) :) <$> optionPairs rest
optionPairs (flag : _) = Left (MissingValue flag)

flags :: [String]
flags = ["-n", "-l", "-f", "-s"]

positiveInt :: String -> String -> Either ArgError Int
positiveInt flag value = case readMaybe value of
    Just n | n > 0 -> Right n
    _ -> Left (InvalidValue flag value)

anyInt :: String -> String -> Either ArgError Int
anyInt flag value =
    maybe (Left (InvalidValue flag value)) Right (readMaybe value)

limit :: String -> String -> Either ArgError Double
limit flag value = case readMaybe (withLeadingZero value) of
    Just l | l >= 0, not (isInfinite l) -> Right l
    _ -> Left (InvalidValue flag value)
  where
    withLeadingZero ('.' : rest) = '0' : '.' : rest
    withLeadingZero s = s

renderArgError :: ArgError -> String
renderArgError err = case err of
    UnknownArgument a -> "unknown argument: " ++ a
    MissingValue flag -> "missing value after " ++ flag
    MissingOption flag -> "missing option " ++ flag
    RepeatedOption flag -> "option " ++ flag ++ " given more than once"
    InvalidValue flag v -> "invalid value for " ++ flag ++ ": " ++ v

usage :: String
usage =
    unlines
        [ "USAGE: ./imageCompressor -n N -l L -f F [-s S]"
        , ""
        , "\tN\tnumber of colors in the final image"
        , "\tL\tconvergence limit"
        , "\tF\tpath to the file containing the colors of the pixels"
        , "\tS\tseed of the random generator (optional)"
        ]
