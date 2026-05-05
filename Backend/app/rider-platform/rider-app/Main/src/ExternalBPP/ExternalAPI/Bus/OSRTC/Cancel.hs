module ExternalBPP.ExternalAPI.Bus.OSRTC.Cancel where

import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Bus.OSRTC.Auth
import ExternalBPP.ExternalAPI.Bus.OSRTC.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
import Tools.Error

type OSRTCFlow m r = (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m)

type GetRefundAmountAPI =
  "api"
    :> "TicketCancellation"
    :> "GetRefundAmount" -- OPRS docs say "GetRefundedAmount" but the server returns 404 on that; only "GetRefundAmount" works
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCGetRefundReq
    :> Post '[JSON] (OSRTCResponse OSRTCGetRefundResList)

type InsertTicketCancelAPI =
  "api"
    :> "TicketCancellation"
    :> "InsertTicketCancel"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] OSRTCCancelReq
    :> Post '[JSON] (OSRTCResponse OSRTCCancelResList)

getRefundAmountAPI :: Proxy GetRefundAmountAPI
getRefundAmountAPI = Proxy

insertTicketCancelAPI :: Proxy InsertTicketCancelAPI
insertTicketCancelAPI = Proxy

bearerAuth :: Text -> Maybe Text
bearerAuth token = Just $ "Bearer " <> token

getRefundAmount :: OSRTCFlow m r => OSRTCConfig -> OSRTCGetRefundReq -> m (OSRTCResponse OSRTCGetRefundResList)
getRefundAmount config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client getRefundAmountAPI (bearerAuth token) req) "OSRTC:GetRefundAmount" getRefundAmountAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_GET_REFUND_AMOUNT_API") config.baseUrl)

insertTicketCancel :: OSRTCFlow m r => OSRTCConfig -> OSRTCCancelReq -> m (OSRTCResponse OSRTCCancelResList)
insertTicketCancel config req = do
  token <- getAuthToken config
  callAPI config.baseUrl (ET.client insertTicketCancelAPI (bearerAuth token) req) "OSRTC:InsertTicketCancel" insertTicketCancelAPI
    >>= fromEitherM (ExternalAPICallError (Just "OSRTC_INSERT_TICKET_CANCEL_API") config.baseUrl)
