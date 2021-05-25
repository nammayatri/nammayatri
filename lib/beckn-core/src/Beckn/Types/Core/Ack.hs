module Beckn.Types.Core.Ack where

import Data.Aeson
import Data.Aeson.Types (unexpected)
import EulerHS.Prelude hiding ((.=))

data AckResponse = Ack
  deriving (Show)

instance FromJSON AckResponse where
  parseJSON = withObject "Ack" $ \v -> do
    status <-
      (v .: "message")
        >>= (.: "ack")
        >>= (.: "status")
    unless (status == String "ACK") (unexpected status)
    pure Ack

instance ToJSON AckResponse where
  toJSON Ack = "message" .== "ack" .== "status" .== String "ACK"
    where
      (.==) :: Text -> Value -> Value
      k .== v = Object (k .= v)
      infixr 9 .==
