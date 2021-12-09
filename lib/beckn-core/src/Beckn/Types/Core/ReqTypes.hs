module Beckn.Types.Core.ReqTypes where

import Beckn.Types.Core.Cabs.Common.Context (Context)
import Beckn.Types.Core.Error (Error)
import qualified Control.Lens as L
import Data.Aeson
import Data.OpenApi
import EulerHS.Prelude hiding ((.=), (.~))
import GHC.Exts (IsList (fromList))

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

instance ToSchema a => ToSchema (BecknCallbackReq a) where
  declareNamedSchema _ = do
    context <- declareSchemaRef (Proxy :: Proxy Context)
    err <- declareSchemaRef (Proxy :: Proxy Error)
    message <- declareSchemaRef (Proxy :: Proxy a)
    let errVariant =
          Inline $
            mempty
              & type_ L.?~ OpenApiObject
              & properties L..~ fromList [("context", context), ("error", err)]
              & required L..~ ["context", "error"]
        messageVariant =
          Inline $
            mempty
              & type_ L.?~ OpenApiObject
              & properties L..~ fromList [("context", context), ("message", message)]
              & required L..~ ["context", "message"]
    return $
      NamedSchema (Just "BecknCallbackReq") $
        mempty
          & type_ L.?~ OpenApiObject
          & oneOf L.?~ [messageVariant, errVariant]

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
