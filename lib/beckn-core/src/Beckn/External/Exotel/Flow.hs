module Beckn.External.Exotel.Flow where

import Beckn.External.Exotel.Types
import Beckn.Types.Common
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant
import Servant.Client
import System.Environment

-- | Exotel API interface
type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] ExotelRequest
    :> Post '[JSON] ExotelResponse

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy

defaultBaseUrl :: ExotelAccountSID -> BaseUrl
defaultBaseUrl sid =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.exotel.com",
      baseUrlPort = 443,
      baseUrlPath =
        T.unpack $
          "/v1/Accounts/"
            <> getExotelAccountSID sid
            <> "/Calls/connect.json"
    }

initiateCall :: T.Text -> T.Text -> FlowR r ()
initiateCall from to = lift . L.forkFlow forkDesc $ do
  apiKey_ <- L.runIO $ lookupEnv "EXOTEL_API_KEY"
  apiToken_ <- L.runIO $ lookupEnv "EXOTEL_API_TOKEN"
  sid_ <- L.runIO $ lookupEnv "EXOTEL_SID"
  callerId_ <- L.runIO $ lookupEnv "EXOTEL_CALLER_ID"
  case (apiKey_, apiToken_, sid_, callerId_) of
    (Just akey, Just atok, Just sd, Just cid) -> do
      let apiKey = ExotelApiKey $ T.pack akey
          apiToken = ExotelApiToken $ T.pack atok
          sid = ExotelAccountSID $ T.pack sd
          callerId = T.pack cid
          exoRequest = ExotelRequest from to callerId
          authData =
            BasicAuthData
              (DT.encodeUtf8 $ getExotelApiKey apiKey)
              (DT.encodeUtf8 $ getExotelApiToken apiToken)
      res <-
        L.callAPI (defaultBaseUrl sid) $
          callExotel authData exoRequest
      L.logInfo exotel $ case res of
        Right _ -> "call initiated from " <> from <> " to " <> to
        Left x -> "error: " <> show x
    _ -> L.logError exotel "exotel ENV vars are not properly set"
  where
    callExotel authData exoRequest = void $ ET.client exotelConnectAPI authData exoRequest
    forkDesc = "Exotel initiate call forked flow " <> from <> " " <> to
    exotel = T.pack "Exotel"
