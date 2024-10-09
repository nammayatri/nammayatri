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
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.ApiV2

type API = ("payout" :> (GetPayoutPayoutReferralHistory :<|> GetPayoutPayoutHistory :<|> PostPayoutPayoutVerifyFraudStatus :<|> PostPayoutPayoutRetryFailed :<|> PostPayoutPayoutRetryAllWithStatus :<|> PostPayoutPayoutPendingPayout))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPayoutPayoutReferralHistory merchantId city :<|> getPayoutPayoutHistory merchantId city :<|> postPayoutPayoutVerifyFraudStatus merchantId city :<|> postPayoutPayoutRetryFailed merchantId city :<|> postPayoutPayoutRetryAllWithStatus merchantId city :<|> postPayoutPayoutPendingPayout merchantId city

type GetPayoutPayoutReferralHistory =
  ( ApiAuthV2
      'DRIVER_OFFER_BPP_MANAGEMENT
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_REFERRAL_HISTORY)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutReferralHistory
  )

type GetPayoutPayoutHistory =
  ( ApiAuthV2
      'DRIVER_OFFER_BPP_MANAGEMENT
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.GET_PAYOUT_PAYOUT_HISTORY)
      :> API.Types.ProviderPlatform.Management.Payout.GetPayoutPayoutHistory
  )

type PostPayoutPayoutVerifyFraudStatus =
  ( ApiAuthV2
      'DRIVER_OFFER_BPP_MANAGEMENT
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_VERIFY_FRAUD_STATUS)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVerifyFraudStatus
  )

type PostPayoutPayoutRetryFailed =
  ( ApiAuthV2
      'DRIVER_OFFER_BPP_MANAGEMENT
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_RETRY_FAILED)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutRetryFailed
  )

type PostPayoutPayoutRetryAllWithStatus =
  ( ApiAuthV2
      'DRIVER_OFFER_BPP_MANAGEMENT
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_RETRY_ALL_WITH_STATUS)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutRetryAllWithStatus
  )

type PostPayoutPayoutPendingPayout =
  ( ApiAuthV2
      'DRIVER_OFFER_BPP_MANAGEMENT
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PAYOUT / 'API.Types.ProviderPlatform.Management.Payout.POST_PAYOUT_PAYOUT_PENDING_PAYOUT)
      :> API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutPendingPayout
  )

getPayoutPayoutReferralHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes)
getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneNo from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneNo from limit offset to

getPayoutPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutHistoryRes)
getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to

postPayoutPayoutVerifyFraudStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.UpdateFraudStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutVerifyFraudStatus merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutVerifyFraudStatus merchantShortId opCity apiTokenInfo req

postPayoutPayoutRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.FailedRetryPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryFailed merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutRetryFailed merchantShortId opCity apiTokenInfo req

postPayoutPayoutRetryAllWithStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.RetryPayoutsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryAllWithStatus merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutRetryAllWithStatus merchantShortId opCity apiTokenInfo req

postPayoutPayoutPendingPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.PendingPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutPendingPayout merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Payout.postPayoutPayoutPendingPayout merchantShortId opCity apiTokenInfo req
