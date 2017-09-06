module Main where

import Database (writeSqlite3)
import Parser (parseChat)

import qualified Data.ByteString.Lazy as ByteString (readFile)
import System.Environment (getArgs)
import System.TimeIt (timeIt)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [chatPath, dbPath] -> do
            putStr "Parsing: "
            parseResult <- timeIt $ parseChat <$> ByteString.readFile chatPath
            case parseResult of
                Left err -> putStrLn err
                Right chat -> do
                    putStr "Writing: "
                    timeIt $ writeSqlite3 dbPath chat
        _ -> putStrLn "Usage: whatsapp-to-db CHATFILE DBFILE"
