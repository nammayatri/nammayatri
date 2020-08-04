{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.FMD.API.Select (OnSelectReq, SelectReq, SelectRes, onSelectAPI)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (encodeToText, fromMaybeM500, throwJsonError500)
import Data.Aeson
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Dunzo.Flow as API
import External.Dunzo.Types
import Product.Dunzo.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Dunzo as Dz
import Storage.Queries.Quote
import Utils.Common (parseBaseUrl)

search :: Organization -> SearchReq -> Flow SearchRes
search org req = do
  config@DunzoConfig {..} <- getDunzoConfig org
  quoteReq <- mkQuoteReq req
  env <- ask
  lift $
    L.forkFlow "Search" $
      flip runReaderT env do
        eres <- getQuote config quoteReq
        case eres of
          Left err ->
            case err of
              FailureResponse _ (Response _ _ _ body) -> sendErrCb org bpId bpNwAddr body
              _ -> L.logDebug @Text "getQuoteErr" (show err)
          Right res -> sendCb org bpId bpNwAddr res
  return $ AckResponse (updateContext (req ^. #context) bpId bpNwAddr) (ack "ACK") Nothing
  where
    sendCb org' bpId bpNwAddr res = do
      onSearchReq <- mkOnSearchReq org' (updateContext (req ^. #context) bpId bpNwAddr) res
      cbUrl <- org' ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
      cbApiKey <- org' ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      cbres <- callCbAPI cbApiKey cbUrl onSearchReq
      L.logDebug @Text "cb" $
        decodeUtf8 (encode onSearchReq)
          <> show cbres

    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onSearchAPI cbApiKey

    sendErrCb org' bpId bpNwAddr errbody =
      case decode errbody of
        Just err -> do
          cbUrl <- org' ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
          cbApiKey <- org' ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
          onSearchErrReq <- mkOnSearchErrReq org' (updateContext (req ^. #context) bpId bpNwAddr) err
          cbres <- callCbAPI cbApiKey cbUrl onSearchErrReq
          L.logDebug @Text "cb" $
            decodeUtf8 (encode onSearchErrReq)
              <> show cbres
        Nothing -> L.logDebug @Text "getQuoteErr" "UNABLE_TO_DECODE_ERR"

select :: Organization -> SelectReq -> Flow SelectRes
select org req = do
  config@DunzoConfig {..} <- getDunzoConfig org
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  env <- ask
  lift $
    L.forkFlow "Select" $
      flip runReaderT env do
        quoteReq <- mkNewQuoteReq req
        eres <- getQuote config quoteReq
        L.logInfo @Text "select" $ show eres
        sendCallback req eres cbUrl cbApiKey
  return $ AckResponse (updateContext (req ^. #context) bpId bpNwAddr) (ack "ACK") Nothing
  where
    sendCallback req' (Right res) cbUrl cbApiKey = do
      (onSelectReq :: OnSelectReq) <- mkOnSelectReq req' res
      let mquote = onSelectReq ^. (#message . #quote)
          quoteId = maybe "" (\q -> q ^. #_id) mquote
          quoteData = encodeToText (onSelectReq ^. #message)
      storeQuote quoteId quoteData
      L.logInfo @Text "on_select" $ "on_select cb req" <> show onSelectReq
      onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
      L.logInfo @Text "on_select" $ "on_select cb resp" <> show onSelectResp
      return ()
    sendCallback req' (Left (FailureResponse _ (Response _ _ _ body))) cbUrl cbApiKey =
      case decode body of
        Just err -> do
          onSelectReq <- mkOnSelectErrReq req' err
          L.logInfo @Text "on_select" $ "on_select cb err req" <> show onSelectReq
          onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
          L.logInfo @Text "on_select" $ "on_select cb err resp" <> show onSelectResp
          return ()
        Nothing -> return ()
    sendCallback _ _ _ _ = return ()

init :: Organization -> InitReq -> Flow InitRes
init _ _ = error "Not implemented yet"

confirm :: Organization -> ConfirmReq -> Flow ConfirmRes
confirm _ _ = error "Not implemented yet"

track :: Organization -> TrackReq -> Flow TrackRes
track _ _ = error "Not implemented yet"

status :: Organization -> StatusReq -> Flow StatusRes
status _ _ = error "Not implemented yet"

cancel :: Organization -> CancelReq -> Flow CancelRes
cancel _ _ = error "Not implemented yet"

getQuote :: DunzoConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote DunzoConfig {..} quoteReq = do
  baseUrl <- parseBaseUrl url
  token <- fetchToken
  API.getQuote clientId token baseUrl quoteReq
  where
    fetchToken = do
      tokenUrl <- parseBaseUrl "http://d4b.dunzodev.in:9016" -- TODO: Fix this, should not be hardcoded
      mToken <- Dz.getToken
      case mToken of
        Nothing -> do
          eres <- API.getToken tokenUrl (TokenReq clientId clientSecret)
          case eres of
            Left err -> throwJsonError500 "TOKEN_ERR" (show err)
            Right (TokenRes token) -> do
              Dz.insertToken token
              return token
        Just token -> return token
