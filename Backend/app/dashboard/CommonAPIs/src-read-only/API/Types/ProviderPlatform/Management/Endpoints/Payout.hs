{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Payout where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.Common
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Servant.Client

data PayoutFlagReason
  = ExceededMaxReferral
  | MinRideDistanceInvalid
  | MinPickupDistanceInvalid
  | CustomerExistAsDriver
  | MultipleDeviceIdExists
  | RideConstraintInvalid
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutReferralHistoryRes = PayoutReferralHistoryRes {history :: [ReferralHistoryItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

data ScheduledPayoutFrequency
  = DAILY
  | WEEKLY
  | MONTHLY
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateScheduledPayoutConfigReq = UpdateScheduledPayoutConfigReq
  { payoutCategory :: Lib.Payment.Domain.Types.Common.EntityName,
    isEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    frequency :: Kernel.Prelude.Maybe ScheduledPayoutFrequency,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dayOfMonth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeOfDay :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    batchSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minimumPayoutAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxRetriesPerDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timeDiffFromUtc :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("payout" :> (GetPayoutPayoutHistory :<|> GetPayoutPayoutReferralHistory :<|> GetPayoutPayout :<|> PostPayoutPayoutRetry :<|> PostPayoutPayoutCancel :<|> PostPayoutPayoutCash :<|> PostPayoutPayoutVpaDelete :<|> PostPayoutPayoutVpaUpdate :<|> PostPayoutPayoutVpaRefundRegistration :<|> PostPayoutPayoutScheduledPayoutConfigUpsert))

type GetPayoutPayoutHistory =
  ( "payout" :> "history" :> QueryParam "driverId" Kernel.Prelude.Text :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "isFailedOnly" Kernel.Prelude.Bool
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutHistoryRes
  )

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

type GetPayoutPayout = ("payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> Get '[JSON] Lib.Payment.API.Payout.Types.PayoutRequestResp)

type PostPayoutPayoutRetry =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "retry"
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCancel =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cancel"
      :> ReqBody
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutCancelReq
      :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCash =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cash"
      :> ReqBody
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutCashUpdateReq
      :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutVpaDelete = ("payout" :> "vpa" :> "delete" :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.DeleteVpaReq :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess)

type PostPayoutPayoutVpaUpdate = ("payout" :> "vpa" :> "update" :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.UpdateVpaReq :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess)

type PostPayoutPayoutVpaRefundRegistration =
  ( "payout" :> "vpa" :> "refundRegistration" :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.RefundRegAmountReq
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutScheduledPayoutConfigUpsert =
  ( "payout" :> "scheduledPayoutConfig" :> "upsert" :> ReqBody '[JSON] UpdateScheduledPayoutConfigReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutHistoryRes,
    getPayoutPayoutReferralHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient PayoutReferralHistoryRes,
    getPayoutPayout :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutRequestResp,
    postPayoutPayoutRetry :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutCancel :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutCash :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaDelete :: Lib.Payment.API.Payout.Types.DeleteVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaUpdate :: Lib.Payment.API.Payout.Types.UpdateVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaRefundRegistration :: Lib.Payment.API.Payout.Types.RefundRegAmountReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutScheduledPayoutConfigUpsert :: UpdateScheduledPayoutConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutHistory :<|> getPayoutPayoutReferralHistory :<|> getPayoutPayout :<|> postPayoutPayoutRetry :<|> postPayoutPayoutCancel :<|> postPayoutPayoutCash :<|> postPayoutPayoutVpaDelete :<|> postPayoutPayoutVpaUpdate :<|> postPayoutPayoutVpaRefundRegistration :<|> postPayoutPayoutScheduledPayoutConfigUpsert = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_HISTORY
  | GET_PAYOUT_PAYOUT_REFERRAL_HISTORY
  | GET_PAYOUT_PAYOUT
  | POST_PAYOUT_PAYOUT_RETRY
  | POST_PAYOUT_PAYOUT_CANCEL
  | POST_PAYOUT_PAYOUT_CASH
  | POST_PAYOUT_PAYOUT_VPA_DELETE
  | POST_PAYOUT_PAYOUT_VPA_UPDATE
  | POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION
  | POST_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG_UPSERT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PayoutUserActionType])
