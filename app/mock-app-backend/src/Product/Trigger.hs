{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Trigger
  ( TriggerFlow (..),
    trigger,
    triggerTrack,
    triggerTrackForLast,
  )
where

import App.Types
import App.Utils
import Beckn.Types.Common
import Beckn.Types.Core.FmdError
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Track
import Beckn.Utils.Common
import Beckn.Utils.Mock
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List (lookup)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import EulerHS.Types (client)
import Product.CallsTrack
import Servant hiding (Context)

data TriggerFlow
  = SimpleConfirm
  | NoSearchResult
  | ServiceUnavailable
  | SearchErrorFMD FmdError
  deriving (Eq, Show, Generic, ToJSON)

instance FromHttpApiData TriggerFlow where
  -- FIXME: there must be some clever way to derive this mechanically
  parseQueryParam flow =
    maybeToRight noMatch $
      lookup flow flows
    where
      flows =
        mconcat
          [ [ ("simple-confirm", SimpleConfirm),
              ("no-search-result", NoSearchResult),
              ("service-unavailable", ServiceUnavailable)
            ],
            M.toList $ SearchErrorFMD <$> allFmdErrorFlowIds
          ]
      noMatch = "Invalid flow. Specify one of: " <> T.intercalate ", " (fst <$> flows)

instance ToHttpApiData TriggerFlow where
  toQueryParam SimpleConfirm = "simple-confirm"
  toQueryParam NoSearchResult = "no-search-result"
  toQueryParam ServiceUnavailable = "service-unavailable"
  toQueryParam (SearchErrorFMD err) = fmdErrorFlowId err
  toHeader = BSL.toStrict . encode
  toUrlPiece = DT.decodeUtf8 . toHeader

trigger :: TriggerFlow -> FlowHandler AckResponse
trigger flow = withFlowHandler $ do
  baseUrl <- xGatewayUri <$> ask
  transactionId <-
    case flow of
      SimpleConfirm -> EL.generateGUID
      NoSearchResult -> pure noSearchResultId
      ServiceUnavailable -> pure serviceUnavailableId
      SearchErrorFMD err -> pure $ fmdErrorFlowId err
  req <- buildSearchReq transactionId
  eRes <-
    callClient "search" baseUrl $
      client searchAPI "test-app-2-key" req
  EL.logDebug @Text "mock_app_backend" $ "search context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = req ^. #context,
        _message = ack "ACK",
        _error = Nothing
      }

triggerTrack :: Text -> FlowHandler AckResponse
triggerTrack orderId = withFlowHandler $ do
  tid <- EL.generateGUID
  context <- buildContext "track" tid
  let reqMsg =
        TrackReqMessage
          { order_id = orderId,
            callback_url = Nothing
          }
  let req = TrackReq context reqMsg
  org <- readingCallsTrack (#orderConfirms . at orderId) >>= fromMaybeM500 "UNKNOWN_ORDER_ID"
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  eRes <-
    callClient "track" cbUrl $
      client trackAPI cbApiKey req
  EL.logDebug @Text "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

triggerTrackForLast :: FlowHandler AckResponse
triggerTrackForLast = do
  lastOrderId <-
    withFlowHandler $
      readingCallsTrack #lastOrderId >>= fromMaybeM400 "NO_ORDERS_REGISTERED"
  triggerTrack lastOrderId
