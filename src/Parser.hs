{-# LANGUAGE RankNTypes, GADTs #-}

module Parser
    ( messageP
    , localTimeP
    ) where

import Chat

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Text.Megaparsec

type Parser = Parsec () Text

messageP :: Parser Message
messageP = do
    dt <- localTimeP
    string " - "
    u <- Text.pack <$> anyChar `someTill` string ": "
    t <- Text.pack <$> anyChar `someTill` ((() <$ nextMessage) <|> eof)
    return Message {dateTime = dt, user = u, message = t}
  where
    nextMessage = try $ char '\n' *> lookAhead messageP

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
