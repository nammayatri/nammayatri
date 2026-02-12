{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Payout
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Payout
import qualified Domain.Action.ProviderPlatform.Management.Payout
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("payout" :> (GetPayoutPayout :<|> PostPayoutPayoutRetry :<|> PostPayoutPayoutCancel :<|> PostPayoutPayoutCash :<|> PostPayoutPayoutVpaDelete :<|> PostPayoutPayoutVpaUpdate :<|> PostPayoutPayoutVpaRefundRegistration))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayout merchantId city :<|> postPayoutPayoutRetry merchantId city :<|> postPayoutPayoutCancel merchantId city :<|> postPayoutPayoutCash merchantId city :<|> postPayoutPayoutVpaDelete merchantId city :<|> postPayoutPayoutVpaUpdate merchantId city :<|> postPayoutPayoutVpaRefundRegistration merchantId city

type GetPayoutPayout =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT))
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayout
  )

type PostPayoutPayoutRetry =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_RETRY))
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutRetry
  )

type PostPayoutPayoutCancel =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_CANCEL))
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutCancel
  )

type PostPayoutPayoutCash =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_CASH))
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutCash
  )

type PostPayoutPayoutVpaDelete =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VPA_DELETE))
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaDelete
  )

type PostPayoutPayoutVpaUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VPA_UPDATE))
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaUpdate
  )

type PostPayoutPayoutVpaRefundRegistration =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PAYOUT) / ('API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION))
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVpaRefundRegistration
  )

getPayoutPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutRequestResp)
getPayoutPayout merchantShortId opCity apiTokenInfo payoutRequestId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayout merchantShortId opCity apiTokenInfo payoutRequestId

postPayoutPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutRetry merchantShortId opCity apiTokenInfo payoutRequestId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutRetry merchantShortId opCity apiTokenInfo payoutRequestId

postPayoutPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCancel merchantShortId opCity apiTokenInfo payoutRequestId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutCancel merchantShortId opCity apiTokenInfo payoutRequestId req

postPayoutPayoutCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCash merchantShortId opCity apiTokenInfo payoutRequestId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutCash merchantShortId opCity apiTokenInfo payoutRequestId req

postPayoutPayoutVpaDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Payment.API.Payout.Types.DeleteVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVpaDelete merchantShortId opCity apiTokenInfo req

postPayoutPayoutVpaUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Payment.API.Payout.Types.UpdateVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVpaUpdate merchantShortId opCity apiTokenInfo req

postPayoutPayoutVpaRefundRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Payment.API.Payout.Types.RefundRegAmountReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaRefundRegistration merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVpaRefundRegistration merchantShortId opCity apiTokenInfo req
