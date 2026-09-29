{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Payout
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Payout
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Payout
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.PayoutBatch
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Storage.Beam.CommonInstances ()

type API = ("payout" :> (GetPayoutPayoutHistory :<|> GetPayoutPayoutReferralHistory :<|> GetPayoutPayoutOrder :<|> GetPayoutPayoutScheduledPayoutConfig :<|> GetPayoutPayout :<|> PostPayoutPayoutRetry :<|> PostPayoutPayoutCancel :<|> PostPayoutPayoutCash :<|> PostPayoutPayoutVpaDelete :<|> PostPayoutPayoutVpaUpdate :<|> PostPayoutPayoutVpaRefundRegistration :<|> PostPayoutPayoutScheduledPayoutConfigUpsert :<|> GetPayoutAdhocLookup :<|> PostPayoutAdhocInitiate :<|> GetPayoutBatchList :<|> GetPayoutBatchOrders :<|> GetPayoutExcluded))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutHistory merchantId city :<|> getPayoutPayoutReferralHistory merchantId city :<|> getPayoutPayoutOrder merchantId city :<|> getPayoutPayoutScheduledPayoutConfig merchantId city :<|> getPayoutPayout merchantId city :<|> postPayoutPayoutRetry merchantId city :<|> postPayoutPayoutCancel merchantId city :<|> postPayoutPayoutCash merchantId city :<|> postPayoutPayoutVpaDelete merchantId city :<|> postPayoutPayoutVpaUpdate merchantId city :<|> postPayoutPayoutVpaRefundRegistration merchantId city :<|> postPayoutPayoutScheduledPayoutConfigUpsert merchantId city :<|> getPayoutAdhocLookup merchantId city :<|> postPayoutAdhocInitiate merchantId city :<|> getPayoutBatchList merchantId city :<|> getPayoutBatchOrders merchantId city :<|> getPayoutExcluded merchantId city

type GetPayoutPayoutHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_HISTORY)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutHistory
  )

type GetPayoutPayoutReferralHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_REFERRAL_HISTORY)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutReferralHistory
  )

type GetPayoutPayoutOrder =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_ORDER)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutOrder
  )

type GetPayoutPayoutScheduledPayoutConfig =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutScheduledPayoutConfig
  )

type GetPayoutPayout =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayout
  )

type PostPayoutPayoutRetry =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_RETRY)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutRetry
  )

type PostPayoutPayoutCancel =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_CANCEL)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutCancel
  )

type PostPayoutPayoutCash =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_CASH)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutCash
  )

type PostPayoutPayoutVpaDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VPA_DELETE)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaDelete
  )

type PostPayoutPayoutVpaUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VPA_UPDATE)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaUpdate
  )

type PostPayoutPayoutVpaRefundRegistration =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaRefundRegistration
  )

type PostPayoutPayoutScheduledPayoutConfigUpsert =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG_UPSERT)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutScheduledPayoutConfigUpsert
  )

type GetPayoutAdhocLookup =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_ADHOC_LOOKUP)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutAdhocLookup
  )

type PostPayoutAdhocInitiate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_ADHOC_INITIATE)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutAdhocInitiate
  )

type GetPayoutBatchList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_BATCH_LIST)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutBatchList
  )

type GetPayoutBatchOrders =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_BATCH_ORDERS)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutBatchOrders
  )

type GetPayoutExcluded =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_EXCLUDED)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutExcluded
  )

getPayoutPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutHistoryRes)
getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to

getPayoutPayoutReferralHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes)
getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneCountryCode driverPhoneNo from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneCountryCode driverPhoneNo from limit offset to

getPayoutPayoutOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutOrderResp)
getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayoutOrder merchantShortId opCity apiTokenInfo payoutOrderId

getPayoutPayoutScheduledPayoutConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.ScheduledPayoutConfigViewResp)
getPayoutPayoutScheduledPayoutConfig merchantShortId opCity apiTokenInfo batchSize bufferCheckEnabled command dayOfMonth dayOfWeek frequency intervalDays intervalHours isEnabled minimumPayoutAmount payoutCategory rescheduleBufferMinutes timeOfDay = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayoutScheduledPayoutConfig merchantShortId opCity apiTokenInfo batchSize bufferCheckEnabled command dayOfMonth dayOfWeek frequency intervalDays intervalHours isEnabled minimumPayoutAmount payoutCategory rescheduleBufferMinutes timeOfDay

getPayoutPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutRequestResp)
getPayoutPayout merchantShortId opCity apiTokenInfo payoutRequestId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayout merchantShortId opCity apiTokenInfo payoutRequestId

postPayoutPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutRetry merchantShortId opCity apiTokenInfo payoutRequestId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutRetry merchantShortId opCity apiTokenInfo payoutRequestId

postPayoutPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCancel merchantShortId opCity apiTokenInfo payoutRequestId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutCancel merchantShortId opCity apiTokenInfo payoutRequestId req

postPayoutPayoutCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCash merchantShortId opCity apiTokenInfo payoutRequestId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutCash merchantShortId opCity apiTokenInfo payoutRequestId req

postPayoutPayoutVpaDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Lib.Payment.API.Payout.Types.DeleteVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVpaDelete merchantShortId opCity apiTokenInfo req

postPayoutPayoutVpaUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Lib.Payment.API.Payout.Types.UpdateVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVpaUpdate merchantShortId opCity apiTokenInfo req

postPayoutPayoutVpaRefundRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Lib.Payment.API.Payout.Types.RefundRegAmountReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaRefundRegistration merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVpaRefundRegistration merchantShortId opCity apiTokenInfo req

postPayoutPayoutScheduledPayoutConfigUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.Payout.UpdateScheduledPayoutConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutScheduledPayoutConfigUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutScheduledPayoutConfigUpsert merchantShortId opCity apiTokenInfo req

getPayoutAdhocLookup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.AdhocPayoutLookupResp)
getPayoutAdhocLookup merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutAdhocLookup merchantShortId opCity apiTokenInfo personId

postPayoutAdhocInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.Payout.AdhocPayoutInitiateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.AdhocPayoutInitiateResp)
postPayoutAdhocInitiate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutAdhocInitiate merchantShortId opCity apiTokenInfo req

getPayoutBatchList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutBatchListRes)
getPayoutBatchList merchantShortId opCity apiTokenInfo from limit offset origin payoutRail status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutBatchList merchantShortId opCity apiTokenInfo from limit offset origin payoutRail status to

getPayoutBatchOrders :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutBatchOrdersRes)
getPayoutBatchOrders merchantShortId opCity apiTokenInfo batchId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutBatchOrders merchantShortId opCity apiTokenInfo batchId

getPayoutExcluded :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutExcludedRes)
getPayoutExcluded merchantShortId opCity apiTokenInfo from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutExcluded merchantShortId opCity apiTokenInfo from limit offset to
