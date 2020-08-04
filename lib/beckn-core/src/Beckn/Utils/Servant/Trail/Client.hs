{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Servant.Trail.Client where

import qualified Beckn.Utils.Servant.Trail.Types as T
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Kind
import EulerHS.Prelude
import Servant
import Servant.Client
import qualified Servant.Client.Core.Request as Client

-- | Request information which we ever want to record.
newtype RequestInfo = RequestInfo
  { _content :: T.RequestContent
  }

-- | Internal type which makes client handlers remember info for tracing.
data ClientTracing verb

-- | Make client handlers return tracing info along with response data.
type family AddClientTracing (api :: Type) :: Type

type instance
  AddClientTracing (subApi :> api) =
    subApi :> AddClientTracing api

type instance
  AddClientTracing (api1 :<|> api2) =
    AddClientTracing api1 :<|> AddClientTracing api2

type instance
  AddClientTracing (Verb method status ctypes res) =
    ClientTracing (Verb method status ctypes res)

withClientTracing :: Proxy api -> Proxy (AddClientTracing api)
withClientTracing _ = Proxy

instance
  HasClient m verb =>
  HasClient m (ClientTracing verb)
  where
  type
    Client m (ClientTracing verb) =
      (RequestInfo, Client m verb)

  hoistClientMonad mp _ hst cli =
    hoistClientMonad mp (Proxy @verb) hst <$> cli

  clientWithRoute mp _ req =
    (toRequestInfo req,) $
      clientWithRoute mp (Proxy @verb) req
    where
      toRequestInfo :: Client.Request -> RequestInfo
      toRequestInfo request =
        RequestInfo
          { _content =
              T.RequestContent
                { _path =
                    T.decodePath $
                      LBS.toStrict . toLazyByteString $ Client.requestPath request,
                  _method = decodeUtf8 $ Client.requestMethod request,
                  _query =
                    toList $ T.decodeQueryParam <$> Client.requestQueryString request,
                  _headers =
                    toList $ T.decodeHeader <$> Client.requestHeaders request,
                  _body =
                    convertReqBody . fst
                      <$> Client.requestBody request
                }
          }

      convertReqBody :: Client.RequestBody -> LByteString
      convertReqBody = \case
        Client.RequestBodyLBS lbs -> lbs
        Client.RequestBodyBS bs -> LBS.fromStrict bs
        Client.RequestBodySource _ ->
          -- This is normally used for e.g. returning file contents,
          -- we don't want this to appear in trails.
          "<IO source>"
