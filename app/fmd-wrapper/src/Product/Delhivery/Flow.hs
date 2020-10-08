{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Delhivery.Flow where

import App.Types
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common
import Data.Aeson
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Delhivery.Flow as API
import External.Delhivery.Types
import Product.Delhivery.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Organization as Org
import Types.Common
import Types.Wrapper

search :: Org.Organization -> SearchReq -> Flow SearchRes
search org req = do
  config@DelhiveryConfig {..} <- dlConfig <$> ask
  quoteReq <- mkQuoteReqFromSearch req
  let context = updateBppUri (req ^. #context) dlBPNwAddress
  bapUrl <- context ^. #_bap_uri & fromMaybeM400 "INVALID_BAP_URI"
  bap <- Org.findByBapUrl bapUrl >>= fromMaybeM400 "BAP_NOT_CONFIGURED"
  dlBACreds <- getDlBAPCreds bap
  fork "Search" $ do
    eres <- getQuote dlBACreds config quoteReq
    sendCb context eres
  returnAck context
  where
    sendCb context res = do
      cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
      cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      case res of
        Right quoteRes -> do
          onSearchReq <- mkOnSearchReq org context quoteRes
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search req") $ encodeToText onSearchReq
          onSearchResp <- L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey onSearchReq
          L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search res") $ show onSearchResp
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              let onSearchErrReq = mkOnSearchErrReq context err
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search err req") $ encodeToText onSearchErrReq
              onSearchResp <- L.callAPI cbUrl $ ET.client onSearchAPI cbApiKey onSearchErrReq
              L.logInfo @Text (req ^. #context . #_transaction_id <> "_on_search err res") $ show onSearchResp
        _ -> pass

fetchToken :: DlBAConfig -> DelhiveryConfig -> Flow Token
fetchToken DlBAConfig {..} DelhiveryConfig {..} = do
  eres <- API.getToken dlTokenUrl (TokenReq dlClientId dlClientSecret "client_credentials")
  case eres of
    Left err -> throwError500 $ show err
    Right tokenRes -> return (tokenRes ^. #access_token)

getQuote :: DlBAConfig -> DelhiveryConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote ba@DlBAConfig {..} conf@DelhiveryConfig {..} quoteReq = do
  token <- getBearerToken <$> fetchToken ba conf
  API.getQuote token dlUrl quoteReq

returnAck :: Context -> Flow AckResponse
returnAck context = return $ AckResponse context (ack "ACK") Nothing

getBearerToken :: Token -> Token
getBearerToken a = Token (T.pack "Bearer " <> getToken a)
