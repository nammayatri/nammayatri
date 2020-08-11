module Beckn.Types.FMD.API.Callback where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Data.Aeson
import EulerHS.Prelude hiding ((.=))

data CallbackReq a = CallbackReq
  { context :: Context,
    contents :: Either Error a
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (CallbackReq a) where
  toJSON (CallbackReq context contents) = object allFields
    where
      contextField = "context" .= context
      allFields = case contents of
        Left err -> contextField : ["error" .= err]
        Right value -> contextField : ["message" .= value]

instance FromJSON a => FromJSON (CallbackReq a) where
  parseJSON = withObject "Request" $ \o ->
    CallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")
