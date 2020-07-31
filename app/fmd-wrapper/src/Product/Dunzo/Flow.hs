{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Error (domainError)
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.FMD.API.Select (SelectReq, SelectRes)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM500, throwJsonError500)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Dunzo.Flow as API
import External.Dunzo.Types
import Product.Dunzo.Transform
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified Storage.Queries.Dunzo as Dz

search :: Organization -> SearchReq -> Flow SearchRes
search org req = do
  (clientId, clientSecret, url, baConfigs) <- getDunzoConfig org
  let baseUrl = getBaseUrl url
  token <- fetchToken baseUrl clientId clientSecret
  quoteReq <- mkQuoteReq req
  eres <- API.getQuote clientId token baseUrl quoteReq
  case eres of
    Left err -> return $ AckResponse (req ^. #context) (ack "NACK") (Just $ domainError $ show err)
    Right res -> do
      onSearchReq <- mkOnSearchReq org (req ^. #context) res
      cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
      cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      sendCb cbApiKey (getCbBaseUrl cbUrl) onSearchReq
      return $ AckResponse (req ^. #context) (ack "ACK") Nothing
  where
    getCbBaseUrl cbUrl =
      BaseUrl
        { baseUrlScheme = Https,
          baseUrlHost = T.unpack cbUrl,
          baseUrlPort = 443,
          baseUrlPath = ""
        }

    sendCb cbApiKey cbUrl req = L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey req

select :: Organization -> SelectReq -> Flow SelectRes
select org req = error "Not implemented yet"

init :: Organization -> InitReq -> Flow InitRes
init org req = error "Not implemented yet"

confirm :: Organization -> ConfirmReq -> Flow ConfirmRes
confirm org req = error "Not implemented yet"

track :: Organization -> TrackReq -> Flow TrackRes
track org req = error "Not implemented yet"

status :: Organization -> StatusReq -> Flow StatusRes
status org req = error "Not implemented yet"

cancel :: Organization -> CancelReq -> Flow CancelRes
cancel org req = error "Not implemented yet"

fetchToken :: BaseUrl -> ClientId -> ClientSecret -> Flow Token
fetchToken baseUrl clientId clientSecret = do
  mToken <- Dz.getToken
  case mToken of
    Nothing -> do
      eres <- callAPI
      case eres of
        Left err -> throwJsonError500 "TOKEN_ERR" (show err)
        Right (TokenRes token) -> do
          Dz.insertToken token
          return token
    Just token -> return token
  where
    callAPI =
      API.getToken baseUrl (TokenReq clientId clientSecret)

getBaseUrl :: Text -> BaseUrl
getBaseUrl url =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = T.unpack url,
      baseUrlPort = 443,
      baseUrlPath = ""
    }
