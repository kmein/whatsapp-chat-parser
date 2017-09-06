module Chat
    ( Message(..)
    , User
    , Chat
    ) where

import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)

type User = Text
type Chat = [Message]

data Message = Message
    { dateTime :: LocalTime
    , user :: Maybe User
    , message :: Text
    } deriving (Eq, Show)
