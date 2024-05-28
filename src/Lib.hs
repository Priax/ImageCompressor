{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-MAR-4-1-compressor-vincent.montero-fontaine
-- File description:
-- Lib
-}
module Lib (someFunc, wordsWhen, exitWithErrorMessage) where
import System.IO (hPutStrLn, stderr)
import System.Exit

someFunc :: IO ()
someFunc = putStrLn "someFunc"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e
