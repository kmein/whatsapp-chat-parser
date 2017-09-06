{-# LANGUAGE RankNTypes, GADTs #-}

module Parser
    ( parseChat
    ) where

import Chat

import Data.Bifunctor (bimap)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import Text.Megaparsec.Char

parseChat :: Text -> Either String Chat
parseChat = bimap show id . parse chatP "(input)"

chatP :: Parser Chat
chatP = many messageP

messageP :: Parser Message
messageP = do
    dt <- localTimeP
    string " - "
    u <- optional $ (Text.toStrict . Text.pack) <$> anyChar `someTill` string ": "
    t <- (Text.toStrict . Text.pack) <$> anyChar `someTill` (eof <|> (() <$ nextMessage))
    return Message {dateTime = dt, user = u, message = t}
  where
    nextMessage = try $ char '\n' *> lookAhead localTimeP

localTimeP :: Parser LocalTime
localTimeP = do
    d <- read <$> dig2
    char '/'
    m <- read <$> dig2
    char '/'
    y <- read <$> count 4 digitChar
    string ", "
    hr <- read <$> dig2
    char ':'
    mi <- read <$> dig2
    let day = fromGregorian y m d
    let time = TimeOfDay hr mi 0
    return (LocalTime day time)
  where
    dig2 = count 2 digitChar
