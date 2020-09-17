{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Trigger
  ( TriggerFlow (..),
    triggerSearch,
    triggerTrack,
    triggerTrackForLast,
    TriggerUpdateMode (..),
    triggerUpdate,
    triggerUpdateForLast,
    triggerCancel,
    triggerCancelForLast,
  )
where

import App.Types
import App.Utils
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.FmdError
import Beckn.Types.FMD.API.Cancel
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Track
import Beckn.Types.FMD.API.Update
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

triggerSearch :: TriggerFlow -> FlowHandler AckResponse
triggerSearch flow = withFlowHandler $ do
  baseUrl <- xGatewayUri <$> ask
  transactionId <-
    case flow of
      SimpleConfirm -> EL.generateGUID
      NoSearchResult -> pure noSearchResultId
      ServiceUnavailable -> pure serviceUnavailableId
      SearchErrorFMD err -> pure $ fmdErrorFlowId err
  req <- buildSearchReq transactionId
  eRes <-
    callClient "search" (req ^. #context) baseUrl $
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
  (cbUrl, cbApiKey) <- getOrderCallbackCoords orderId
  eRes <-
    callClient "track" context cbUrl $
      client trackAPI cbApiKey req
  EL.logDebug @Text "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

triggerTrackForLast :: FlowHandler AckResponse
triggerTrackForLast = getLastOrderId >>= triggerTrack

triggerCancel :: Text -> FlowHandler AckResponse
triggerCancel orderId = withFlowHandler $ do
  tid <- EL.generateGUID
  context <- buildContext "cancel" tid
  let reqMsg =
        CancelReqMessage $
          CancelOrder
            { id = orderId,
              cancellation_reason_id = "Test cancel triggered"
            }
  let req = CancelReq context reqMsg
  (cbUrl, cbApiKey) <- getOrderCallbackCoords orderId
  eRes <-
    callClient "cancel" context cbUrl $
      client cancelAPI cbApiKey req
  EL.logDebug @Text "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

triggerCancelForLast :: FlowHandler AckResponse
triggerCancelForLast = getLastOrderId >>= triggerCancel

data TriggerUpdateMode
  = LeaveIntact
  | SetFarDropLocation

instance FromHttpApiData TriggerUpdateMode where
  parseQueryParam mode =
    maybeToRight noMatch $
      lookup mode modes
    where
      modes =
        [ ("set-far-drop-location", SetFarDropLocation),
          ("leave-intact", SetFarDropLocation)
        ]
      noMatch = "Invalid mode. Specify one of: " <> T.intercalate ", " (fst <$> modes)

instance ToHttpApiData TriggerUpdateMode where
  toQueryParam = \case
    SetFarDropLocation -> "set-far-drop-location"
    LeaveIntact -> "leave-intact"

triggerUpdate :: Text -> TriggerUpdateMode -> FlowHandler AckResponse
triggerUpdate orderId mode = withFlowHandler $ do
  tid <- EL.generateGUID
  context <- buildContext "update" tid
  let req = case mode of
        LeaveIntact ->
          UpdateReq
            context
            UpdateReqMessage
              { order = example,
                update_action = "update_drop_instructions"
              }
        SetFarDropLocation ->
          UpdateReq
            context {_transaction_id = locationTooFarId}
            UpdateReqMessage
              { order = example,
                update_action = "update_drop_location"
              }
  (cbUrl, cbApiKey) <- getOrderCallbackCoords orderId
  eRes <-
    callClient "update" context cbUrl $
      client updateAPI cbApiKey req
  EL.logDebug @Text "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

triggerUpdateForLast :: TriggerUpdateMode -> FlowHandler AckResponse
triggerUpdateForLast mode = do
  orderId <- getLastOrderId
  triggerUpdate orderId mode

getOrderCallbackCoords :: Text -> Flow (BaseUrl, Text)
getOrderCallbackCoords orderId = do
  OrderInfo {bppOrg = org} <- readingCallsTrack (#orderConfirms . at orderId) >>= fromMaybeM500 "UNKNOWN_ORDER_ID"
  cbUrl <- org ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  return (cbUrl, cbApiKey)

getLastOrderId :: FlowHandler Text
getLastOrderId =
  withFlowHandler $
    readingCallsTrack #lastOrderId >>= fromMaybeM400 "NO_ORDERS_REGISTERED"
