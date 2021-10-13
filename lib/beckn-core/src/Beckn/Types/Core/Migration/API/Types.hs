module Beckn.Types.Core.Migration.API.Types where

import Beckn.Types.Core.Migration.Context (Context)
import Beckn.Types.Core.Migration.Error (Error)
import Beckn.Types.Core.Migration.Order (Order)
import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data BecknReq a = BecknReq
  { context :: Context,
    message :: a
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema a => ToSchema (BecknReq a)

data BecknCallbackReq a = BecknCallbackReq
  { context :: Context,
    contents :: Either Error a
  }
  deriving (Generic, Show)

instance ToSchema a => ToSchema (BecknCallbackReq a)

instance ToJSON a => ToJSON (BecknCallbackReq a) where
  toJSON (BecknCallbackReq context contents) = object $ contextField : errorOrMessage
    where
      contextField = "context" .= context
      errorOrMessage = case contents of
        Left err -> ["error" .= err]
        Right message -> ["message" .= message]

instance FromJSON a => FromJSON (BecknCallbackReq a) where
  parseJSON = withObject "BecknCallbackReq" $ \o ->
    BecknCallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")

newtype OrderObject = OrderObject
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)
