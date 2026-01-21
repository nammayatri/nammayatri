{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.RideBooking.Payout
  ( getPayoutStatus,
    postPayoutCancel,
    postPayoutRetry,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking
import qualified API.Types.Dashboard.RideBooking.Payout
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant
import qualified Dashboard.Common as Common

getPayoutStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow API.Types.Dashboard.RideBooking.Payout.PayoutStatusResp)
getPayoutStatus merchantShortId opCity rideId = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.payoutDSL.getPayoutStatus) rideId

postPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.Dashboard.RideBooking.Payout.PayoutCancelReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutCancel merchantShortId opCity apiTokenInfo rideId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing (Kernel.Prelude.Just (Kernel.Types.Id.Id rideId :: Kernel.Types.Id.Id Common.Ride)) (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.payoutDSL.postPayoutCancel) rideId req)

postPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutRetry merchantShortId opCity apiTokenInfo rideId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing (Kernel.Prelude.Just (Kernel.Types.Id.Id rideId :: Kernel.Types.Id.Id Common.Ride)) SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.payoutDSL.postPayoutRetry) rideId)
