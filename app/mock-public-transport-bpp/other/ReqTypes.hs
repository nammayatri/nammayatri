-- module Beckn.Types.Core.ReqTypes where

module Core.ReqTypes where

import Beckn.Prelude
import Core.Context

data BecknReq a = BecknReq
  { context :: Context,
    message :: a
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- instance ToSchema a => ToSchema (BecknReq a)

-- data BecknCallbackReq a = BecknCall
-- { context :: Context,
-- contents :: Either Error a
-- }
-- deriving (Generic, Show)
--
-- instance ToJSON a => ToJSON (BecknCallbackReq a) where
-- toJSON (BecknCallbackReq context contents) = object $ contex
-- where
-- contextField = "context" .= context
-- errorOrMessage = case contents of
-- Left err -> ["error" .= err]
-- Right message -> ["message" .= message]
--
-- instance FromJSON a => FromJSON (BecknCallbackReq a) where
-- parseJSON = withObject "BecknCallbackReq" $ \o ->
-- BecknCallbackReq
-- <$> o .: "context"
-- <*> (Left <$> o .: "error" <|> Right <$> o .: "message")
