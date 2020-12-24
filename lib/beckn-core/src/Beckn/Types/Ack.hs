module Beckn.Types.Ack (Ack, ackSuccess, ackFailure) where

import Data.Aeson (object, (.=))
import EulerHS.Prelude hiding ((.=))

-- Don't export constructor and records. Use smart constructors below, add more if you need it.
data Ack = Ack
  { result :: Result,
    message :: Text
  }

instance ToJSON Ack where
  toJSON (Ack result message)
    | null message = object ["result" .= result]
    | otherwise = object ["result" .= result, "message" .= message]

data Result = Success | Failure
  deriving (Generic, ToJSON)

ackSuccess :: Ack
ackSuccess =
  Ack
    { result = Success,
      message = ""
    }

ackFailure :: Text -> Ack
ackFailure msg =
  Ack
    { result = Failure,
      message = msg
    }
