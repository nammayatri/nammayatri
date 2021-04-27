{-# LANGUAGE OverloadedLabels #-}

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
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.FmdError
import qualified Beckn.Types.FMD.API.Cancel as API
import qualified Beckn.Types.FMD.API.Search as API
import qualified Beckn.Types.FMD.API.Track as API
import qualified Beckn.Types.FMD.API.Update as API
import Beckn.Utils.Common
import Beckn.Utils.Example
import Beckn.Utils.Mock
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
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
import Types.Error hiding (ServiceUnavailable)

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
triggerSearch flow = withFlowHandlerBecknAPI $ do
  baseUrl <- xGatewayUri <$> ask
  transactionId <-
    case flow of
      SimpleConfirm -> EL.generateGUID
      NoSearchResult -> pure noSearchResultId
      ServiceUnavailable -> pure serviceUnavailableId
      SearchErrorFMD err -> pure $ fmdErrorFlowId err
  req <- buildSearchReq transactionId
  eRes <-
    callClient' (Just HttpSig.signatureAuthManagerKey) "search" (req ^. #context) baseUrl $
      client API.searchAPI req
  logTagDebug "mock_app_backend" $ "search context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = req ^. #context,
        _message = ack ACK,
        _error = Nothing
      }

triggerTrack :: Text -> FlowHandler AckResponse
triggerTrack orderId = withFlowHandlerBecknAPI $ do
  tid <- EL.generateGUID
  context <- buildContext "track" tid
  let reqMsg =
        API.TrackReqMessage
          { order_id = orderId,
            callback_url = Nothing
          }
  let req = API.TrackReq context reqMsg
  (cbUrl, _) <- getOrderCallbackCoords orderId
  eRes <-
    callClient' (Just HttpSig.signatureAuthManagerKey) "track" context cbUrl $
      client API.trackAPI req
  logTagDebug "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack ACK,
        _error = Nothing
      }

triggerTrackForLast :: FlowHandler AckResponse
triggerTrackForLast = getLastOrderId >>= triggerTrack

triggerCancel :: Text -> FlowHandler AckResponse
triggerCancel orderId = withFlowHandlerBecknAPI $ do
  tid <- EL.generateGUID
  context <- buildContext "cancel" tid
  let reqMsg =
        API.CancelReqMessage $
          API.CancelOrder
            { id = orderId,
              cancellation_reason_id = "Test cancel triggered"
            }
  let req = API.CancelReq context reqMsg
  (cbUrl, _) <- getOrderCallbackCoords orderId
  eRes <-
    callClient' (Just HttpSig.signatureAuthManagerKey) "cancel" context cbUrl $
      client API.cancelAPI req
  logTagDebug "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack ACK,
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
triggerUpdate orderId mode = withFlowHandlerBecknAPI $ do
  tid <- EL.generateGUID
  context <- buildContext "update" tid
  let req = case mode of
        LeaveIntact ->
          API.UpdateReq
            context
            API.UpdateReqMessage
              { order = example,
                update_action = "update_drop_instructions"
              }
        SetFarDropLocation ->
          API.UpdateReq
            context {_transaction_id = locationTooFarId}
            API.UpdateReqMessage
              { order = example,
                update_action = "update_drop_location"
              }
  (cbUrl, _) <- getOrderCallbackCoords orderId
  eRes <-
    callClient' (Just HttpSig.signatureAuthManagerKey) "update" context cbUrl $
      client API.updateAPI req
  logTagDebug "mock_app_backend" $ "track context: " <> show (toJSON $ eRes ^. #_context) <> ", resp: " <> show (toJSON $ eRes ^. #_message)
  return
    AckResponse
      { _context = context,
        _message = ack ACK,
        _error = Nothing
      }

triggerUpdateForLast :: TriggerUpdateMode -> FlowHandler AckResponse
triggerUpdateForLast mode = do
  orderId <- getLastOrderId
  triggerUpdate orderId mode

getOrderCallbackCoords :: Text -> Flow (BaseUrl, Text)
getOrderCallbackCoords orderId = do
  OrderInfo {bppOrg = org} <-
    readingCallsTrack (#orderConfirms . at orderId)
      >>= fromMaybeM (InternalError "Unknown order id.")
  cbUrl <- org ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM (OrgFieldNotPresent "callback_api_key")
  return (cbUrl, cbApiKey)

getLastOrderId :: FlowHandler Text
getLastOrderId =
  withFlowHandlerAPI $
    readingCallsTrack #lastOrderId >>= fromMaybeM (InvalidRequest "No orders registered.")
