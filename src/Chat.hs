module Chat
    ( Message(..)
    , Direction(..)
    , User
    , Color
    , Chat
    ) where

import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)

type User = Text
type Color = Text
type Chat = [Message]

data Direction = Incoming | Outgoing

data Message = Message
    { dateTime :: LocalTime
    , user :: User
    , message :: Text
    } deriving (Eq, Show)
