{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Payout where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
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

data DeleteVpaReq = DeleteVpaReq {driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteVpaReq where
  hideSecrets = Kernel.Prelude.identity

data EntityName
  = MANUAL
  | DRIVER_DAILY_STATS
  | BACKLOG
  | DAILY_STATS_VIA_DASHBOARD
  | RETRY_VIA_DASHBOARD
  | DRIVER_FEE
  | INVALID
  | DRIVER_WALLET_TRANSACTION
  | DRIVER_WALLET_TOPUP
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
  | RideConstraintInvalid
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutHistoryItem = PayoutHistoryItem
  { driverName :: Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Text,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    payoutAmount :: Kernel.Types.Common.HighPrecMoney,
    payoutStatus :: Kernel.Prelude.Text,
    payoutTime :: Data.Time.LocalTime,
    payoutEntity :: Kernel.Prelude.Maybe EntityName,
    payoutOrderId :: Kernel.Prelude.Text,
    responseMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutRetriedOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutHistoryRes = PayoutHistoryRes {history :: [PayoutHistoryItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutPeriod
  = SinglePayout
  | DailyPayout Kernel.Prelude.Int
  | WeeklyPayout Kernel.Prelude.Int
  | MonthlyPayout Kernel.Prelude.Int
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutReferralHistoryRes = PayoutReferralHistoryRes {history :: [ReferralHistoryItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutSchedulerReq = PayoutSchedulerReq
  { runNewJob :: Kernel.Prelude.Bool,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    payoutPeriod :: Kernel.Prelude.Maybe PayoutPeriod,
    driversInBatch :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eachDriverDelayMs :: Kernel.Prelude.Maybe Kernel.Types.Common.Milliseconds,
    batchDelayS :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    completeExistingJob :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Job)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PayoutSchedulerReq where
  hideSecrets = Kernel.Prelude.identity

data PayoutSchedulerResp = PayoutSchedulerResp {result :: PayoutSchedulerResult, jobId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Job), existingJobs :: [Kernel.Types.Id.Id Dashboard.Common.Job]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PayoutSchedulerResp where
  hideSecrets = Kernel.Prelude.identity

data PayoutSchedulerResult
  = Success
  | Failed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutVpaStatus
  = VIA_WEBHOOK
  | MANUALLY_ADDED
  | VERIFIED_BY_USER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PendingPayoutReq = PendingPayoutReq {personId :: Kernel.Types.Id.Id Dashboard.Common.Driver}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PendingPayoutReq where
  hideSecrets = Kernel.Prelude.identity

data ReferralHistoryItem = ReferralHistoryItem
  { referralDate :: Kernel.Prelude.UTCTime,
    customerPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderDetailsId :: Kernel.Prelude.Text,
    hasTakenValidActivatedRide :: Kernel.Prelude.Bool,
    dateOfActivation :: Kernel.Prelude.Maybe Data.Time.LocalTime,
    fraudFlaggedReason :: Kernel.Prelude.Maybe PayoutFlagReason,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride),
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver),
    isReviewed :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundRegAmountReq = RefundRegAmountReq {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RefundRegAmountReq where
  hideSecrets = Kernel.Prelude.identity

data RetryPayoutsReq = RetryPayoutsReq {limit :: Kernel.Prelude.Int, offset :: Kernel.Prelude.Int, status :: Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus, entityNames :: [EntityName]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RetryPayoutsReq where
  hideSecrets = Kernel.Prelude.identity

data SetDriversBlockStateReq = SetDriversBlockStateReq {driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver], blockState :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SetDriversBlockStateReq where
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

data UpdateVpaReq = UpdateVpaReq {driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, vpa :: Kernel.Prelude.Text, vpaStatus :: PayoutVpaStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVpaReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("payout" :> (GetPayoutPayoutReferralHistory :<|> GetPayoutPayoutHistory :<|> PostPayoutPayoutVerifyFraudStatus :<|> PostPayoutPayoutRetryFailed :<|> PostPayoutPayoutRetryAllWithStatus :<|> PostPayoutPayoutPendingPayout :<|> PostPayoutPayoutDeleteVPA :<|> PostPayoutPayoutDriversSetBlockState :<|> PostPayoutPayoutUpdateVPA :<|> PostPayoutPayoutRefundRegistrationAmount :<|> PostPayoutScheduler))

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
           PayoutReferralHistoryRes
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
           PayoutHistoryRes
  )

type PostPayoutPayoutVerifyFraudStatus = ("payout" :> "verifyFraudStatus" :> ReqBody '[JSON] UpdateFraudStatusReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutRetryFailed = ("payout" :> "retryFailed" :> ReqBody '[JSON] FailedRetryPayoutReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutRetryAllWithStatus = ("payout" :> "retryAllWithStatus" :> ReqBody '[JSON] RetryPayoutsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutPendingPayout = ("payout" :> "pendingPayout" :> ReqBody '[JSON] PendingPayoutReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutDeleteVPA = ("payout" :> "deleteVPA" :> ReqBody '[JSON] DeleteVpaReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutDriversSetBlockState = ("payout" :> "drivers" :> "setBlockState" :> ReqBody '[JSON] SetDriversBlockStateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutUpdateVPA = ("payout" :> "updateVPA" :> ReqBody '[JSON] UpdateVpaReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutPayoutRefundRegistrationAmount = ("payout" :> "refundRegistrationAmount" :> ReqBody '[JSON] RefundRegAmountReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostPayoutScheduler = ("scheduler" :> ReqBody '[JSON] PayoutSchedulerReq :> Post '[JSON] PayoutSchedulerResp)

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutReferralHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient PayoutReferralHistoryRes,
    getPayoutPayoutHistory :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient PayoutHistoryRes,
    postPayoutPayoutVerifyFraudStatus :: UpdateFraudStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutRetryFailed :: FailedRetryPayoutReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutRetryAllWithStatus :: RetryPayoutsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutPendingPayout :: PendingPayoutReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutDeleteVPA :: DeleteVpaReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutDriversSetBlockState :: SetDriversBlockStateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutUpdateVPA :: UpdateVpaReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutPayoutRefundRegistrationAmount :: RefundRegAmountReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPayoutScheduler :: PayoutSchedulerReq -> EulerHS.Types.EulerClient PayoutSchedulerResp
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutReferralHistory :<|> getPayoutPayoutHistory :<|> postPayoutPayoutVerifyFraudStatus :<|> postPayoutPayoutRetryFailed :<|> postPayoutPayoutRetryAllWithStatus :<|> postPayoutPayoutPendingPayout :<|> postPayoutPayoutDeleteVPA :<|> postPayoutPayoutDriversSetBlockState :<|> postPayoutPayoutUpdateVPA :<|> postPayoutPayoutRefundRegistrationAmount :<|> postPayoutScheduler = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_REFERRAL_HISTORY
  | GET_PAYOUT_PAYOUT_HISTORY
  | POST_PAYOUT_PAYOUT_VERIFY_FRAUD_STATUS
  | POST_PAYOUT_PAYOUT_RETRY_FAILED
  | POST_PAYOUT_PAYOUT_RETRY_ALL_WITH_STATUS
  | POST_PAYOUT_PAYOUT_PENDING_PAYOUT
  | POST_PAYOUT_PAYOUT_DELETE_VPA
  | POST_PAYOUT_PAYOUT_DRIVERS_SET_BLOCK_STATE
  | POST_PAYOUT_PAYOUT_UPDATE_VPA
  | POST_PAYOUT_PAYOUT_REFUND_REGISTRATION_AMOUNT
  | POST_PAYOUT_SCHEDULER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PayoutUserActionType])
