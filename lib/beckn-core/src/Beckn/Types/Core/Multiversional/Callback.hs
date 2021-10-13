module Beckn.Types.Core.Multiversional.Callback where

import Beckn.Types.App ()
import Beckn.Types.Core.Error
import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

-- Creating own gateway CallbackReq to support Context for 0.8 and 0.9

data CallbackReq = CallbackReq
  { context :: Value,
    contents :: Either Error Value
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON CallbackReq where
  toJSON (CallbackReq context contents) = object allFields
    where
      contextField = "context" .= context
      allFields = case contents of
        Left err -> contextField : ["error" .= err]
        Right message -> contextField : ["message" .= message]

instance FromJSON CallbackReq where
  parseJSON = withObject "CallbackReq" $ \o ->
    CallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")
