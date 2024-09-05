{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Payout where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payout.Juspay.Types.Payout
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data EntityName
  = MANUAL
  | DRIVER_DAILY_STATS
  | BACKLOG
  | DAILY_STATS_VIA_DASHBOARD
  | RETRY_VIA_DASHBOARD
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FailedRetryPayoutReq = FailedRetryPayoutReq {payoutOrderId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FailedRetryPayoutReq where
  hideSecrets = Kernel.Prelude.identity

data PayoutFlagReason
  = ExceededMaxReferral
  | MinRideDistanceInvalid
  | MinPickupDistanceInvalid
  | CustomerExistAsDriver
  | MultipleDeviceIdExists
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutHistoryItem = PayoutHistoryItem
  { driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    payoutAmount :: Kernel.Types.Common.HighPrecMoney,
    payoutStatus :: Kernel.Prelude.Text,
    payoutTime :: Data.Time.LocalTime,
    payoutEntity :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Payout.EntityName,
    payoutOrderId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutHistoryRes = PayoutHistoryRes {history :: [API.Types.ProviderPlatform.Management.Payout.PayoutHistoryItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutReferralHistoryRes = PayoutReferralHistoryRes {history :: [API.Types.ProviderPlatform.Management.Payout.ReferralHistoryItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReferralHistoryItem = ReferralHistoryItem
  { referralDate :: Kernel.Prelude.UTCTime,
    customerPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderDetailsId :: Kernel.Prelude.Text,
    hasTakenValidActivatedRide :: Kernel.Prelude.Bool,
    dateOfActivation :: Kernel.Prelude.Maybe Data.Time.LocalTime,
    fraudFlaggedReason :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Payout.PayoutFlagReason,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride),
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver),
    isReviewed :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RetryPayoutsReq = RetryPayoutsReq
  { limit :: Kernel.Prelude.Int,
    offset :: Kernel.Prelude.Int,
    status :: Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus,
    entityNames :: [API.Types.ProviderPlatform.Management.Payout.EntityName]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RetryPayoutsReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateFraudStatusReq = UpdateFraudStatusReq
  { isFlagConfirmed :: Kernel.Prelude.Bool,
    firstRideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    riderDetailsId :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateFraudStatusReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("payout" :> (GetPayoutPayoutReferralHistory :<|> GetPayoutPayoutHistory :<|> PostPayoutPayoutVerifyFraudStatus :<|> PostPayoutPayoutRetryFailed :<|> PostPayoutPayoutRetryAllWithStatus))

type GetPayoutPayoutReferralHistory =
  ( "payout" :> "referral" :> "history" :> QueryParam "areActivatedRidesOnly" Kernel.Prelude.Bool
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam
           "driverPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes
  )

type GetPayoutPayoutHistory =
  ( "payout" :> "history" :> QueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "isFailedOnly" Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Payout.PayoutHistoryRes
  )

type PostPayoutPayoutVerifyFraudStatus =
  ( "payout" :> "verifyFraudStatus" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Payout.UpdateFraudStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPayoutPayoutRetryFailed =
  ( "payout" :> "retryFailed" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Payout.FailedRetryPayoutReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPayoutPayoutRetryAllWithStatus =
  ( "payout" :> "retryAllWithStatus" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Payout.RetryPayoutsReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutReferralHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes,
    getPayoutPayoutHistory :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Payout.PayoutHistoryRes,
    postPayoutPayoutVerifyFraudStatus :: API.Types.ProviderPlatform.Management.Payout.UpdateFraudStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutRetryFailed :: API.Types.ProviderPlatform.Management.Payout.FailedRetryPayoutReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutRetryAllWithStatus :: API.Types.ProviderPlatform.Management.Payout.RetryPayoutsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutReferralHistory :<|> getPayoutPayoutHistory :<|> postPayoutPayoutVerifyFraudStatus :<|> postPayoutPayoutRetryFailed :<|> postPayoutPayoutRetryAllWithStatus = payoutClient

data PayoutEndpointDSL
  = GetPayoutPayoutReferralHistoryEndpoint
  | GetPayoutPayoutHistoryEndpoint
  | PostPayoutPayoutVerifyFraudStatusEndpoint
  | PostPayoutPayoutRetryFailedEndpoint
  | PostPayoutPayoutRetryAllWithStatusEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
