module Beckn.Types.Core.ReqTypes where

import Beckn.Types.Core.Error (Error)
import Beckn.Types.Core.Taxi.Common.Context (Context)
import Beckn.Utils.GenericPretty
import qualified Control.Lens as L
import Data.Aeson
import Data.OpenApi
import Data.Typeable
import EulerHS.Prelude hiding ((.=), (.~))
import GHC.Exts (IsList (fromList))

data BecknReq a = BecknReq
  { context :: Context,
    message :: a
  }
  deriving (Generic, Show, FromJSON, ToJSON, PrettyShow)

instance ToSchema a => ToSchema (BecknReq a)

data BecknCallbackReq a = BecknCallbackReq
  { context :: Context,
    contents :: Either Error a
  }
  deriving (Generic, Show, PrettyShow)

instance (ToSchema a) => ToSchema (BecknCallbackReq a) where
  declareNamedSchema _ = do
    context <- declareSchemaRef (Proxy :: Proxy Context)
    err <- declareSchemaRef (Proxy :: Proxy Error)
    let messageTypeName = show $ typeRep (Proxy :: Proxy a)
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
      NamedSchema (Just $ "BecknCallbackReq_" <> messageTypeName) $
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
