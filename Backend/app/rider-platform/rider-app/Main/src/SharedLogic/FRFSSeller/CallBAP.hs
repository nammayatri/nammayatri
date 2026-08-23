module SharedLogic.FRFSSeller.CallBAP (sendOnSearch, sendOnSelect, sendOnInit, sendOnConfirm, sendOnStatus, sendOnCancel, sendOnInfo, sendOnReceiverRecon) where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.NTS10.APIs as NTS10
import qualified BecknV2.NTS10.Types as NTS10
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import qualified SharedLogic.CallFRFSBPP as CallBPP

sendOnSearch :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnSearchReq -> Flow ()
sendOnSearch merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_search" Spec.onSearchAPI

sendOnSelect :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnSelectReq -> Flow ()
sendOnSelect merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_select" Spec.onSelectAPI

sendOnInit :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnInitReq -> Flow ()
sendOnInit merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_init" Spec.onInitAPI

sendOnConfirm :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnConfirmReq -> Flow ()
sendOnConfirm merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_confirm" Spec.onConfirmAPI

sendOnStatus :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnStatusReq -> Flow ()
sendOnStatus merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_status" Spec.onStatusAPI

sendOnCancel :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnCancelReq -> Flow ()
sendOnCancel merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_cancel" Spec.onCancelAPI

sendOnInfo :: Id DM.Merchant -> Text -> BaseUrl -> Spec.OnInfoReq -> Flow ()
sendOnInfo merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_info" Spec.onInfoAPI

sendOnReceiverRecon :: Id DM.Merchant -> Text -> BaseUrl -> NTS10.OnReceiverReconReq -> Flow ()
sendOnReceiverRecon merchantId selfSubscriberId bapUri =
  sendCallback merchantId selfSubscriberId bapUri "on_receiver_recon" NTS10.onReceiverReconAPI

sendCallback ::
  (IsBecknAPI api req res, SanitizedUrl api, ToJSON req) =>
  Id DM.Merchant ->
  Text ->
  BaseUrl ->
  Text ->
  Proxy api ->
  req ->
  Flow ()
sendCallback merchantId selfSubscriberId bapUri action api req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  result <-
    withTryCatch ("frfsSeller:send" <> action) . withShortRetry $
      CallBPP.callBecknAPIWithSignature'
        merchantId
        selfSubscriberId
        action
        api
        bapUri
        internalEndPointHashMap
        req
  case result of
    Right _ -> logInfo $ action <> " delivered to " <> showBaseUrl bapUri
    Left err -> logError $ action <> " NOT delivered after retries to " <> showBaseUrl bapUri <> ": " <> show err
