module DebugRequest where

import Beckn.Prelude
import Control.Concurrent
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as DCI
import Data.String.Conversions
import GenericPretty
import qualified Network.Wai as NW

data DebugRequest = DebugRequest
  { requestMethod :: BS.ByteString,
    rawPathInfo :: BS.ByteString,
    rawQueryString :: BS.ByteString,
    requestHeaders :: [HeaderUnit],
    --  , remoteHost
    --  , pathInfo
    --  , queryString
    requestBody :: Either BSL.ByteString Value
    --  , requestBodyLength
    --  , requestHeaderHost
  }
  deriving (Generic, PrettyShow)

data HeaderUnit = HeaderUnit
  { headerName :: BS.ByteString,
    headerValue :: BS.ByteString
  }
  deriving (Generic)

instance PrettyShow HeaderUnit where
  prettyShow HeaderUnit {..} = layoutStr $ cs $ mconcat [headerName, " = \"", headerValue]

toDebugRequest :: NW.Request -> IO DebugRequest
toDebugRequest req = do
  let requestMethod = NW.requestMethod req
      rawPathInfo = NW.rawPathInfo req
      rawQueryString = NW.rawQueryString req
      requestHeaders = map (uncurry HeaderUnit . first DCI.original) $ NW.requestHeaders req
  requestBodyRaw <- NW.strictRequestBody req
  let requestBody = maybe (Left requestBodyRaw) Right $ decode requestBodyRaw
  pure $ DebugRequest {..}

logRequest :: NW.Application -> NW.Application
logRequest f req respF = do
  req' <- do
    body <- NW.strictRequestBody req <&> BSL.toStrict
    mvar <- newMVar body
    pure req {NW.requestBody = mkRequestBody mvar}
  debugReq <- toDebugRequest req'
  putStrLn $ "request :" <> defaultPretty debugReq
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe BS.empty
