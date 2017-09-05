{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chat (Message(..), Direction(..))
import LaTeX (latexChat)
import Parser (messageP)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (getArgs)
import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.Megaparsec (parseMaybe, many)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [u] -> do
            doc <- Text.pack <$> readFile "chat.txt"
            case parseMaybe (many messageP) doc of
                Nothing -> putStrLn "Parsing failed"
                Just ms ->
                    let addDirections =
                            map
                                (\m ->
                                     if Text.unpack (user m) == u
                                         then (Outgoing, m)
                                         else (Incoming, m))
                    in Text.putStrLn $ latexChat Nothing $ addDirections ms
        _ -> putStrLn "please pass a username"
