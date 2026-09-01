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
import qualified "payment" Lib.Payment.Domain.Types.PayoutBatch
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Servant.Client

data AdhocPayoutEligibilityResp = AdhocPayoutEligibilityResp
  { personId :: Kernel.Prelude.Text,
    personName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    role :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    walletBalance :: Kernel.Types.Common.HighPrecMoney,
    nonRedeemableAmount :: Kernel.Types.Common.HighPrecMoney,
    payoutableBalance :: Kernel.Types.Common.HighPrecMoney,
    minimumPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    isEligible :: Kernel.Prelude.Bool,
    payoutServiceFlow :: Kernel.Prelude.Text,
    bankAccountStatus :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdhocPayoutInitiateReq = AdhocPayoutInitiateReq {personIds :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdhocPayoutInitiateResp = AdhocPayoutInitiateResp {results :: [AdhocPayoutResultItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdhocPayoutItemStatus
  = INITIATED
  | SKIPPED
  | FAILED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdhocPayoutResultItem = AdhocPayoutResultItem
  { personId :: Kernel.Prelude.Text,
    status :: AdhocPayoutItemStatus,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutBatchListItem = PayoutBatchListItem
  { id :: Kernel.Prelude.Text,
    runId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    runSummary :: Kernel.Prelude.Maybe PayoutRunSummary,
    origin :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin,
    status :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus,
    payoutRail :: Kernel.Prelude.Text,
    valueDate :: Data.Time.Day,
    clientRefNo :: Kernel.Prelude.Text,
    partnerBatchRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    itemCount :: Kernel.Prelude.Int,
    totalAmount :: Kernel.Types.Common.HighPrecMoney,
    processedCount :: Kernel.Prelude.Int,
    rejectedCount :: Kernel.Prelude.Int,
    pendingCount :: Kernel.Prelude.Int,
    partnerResponseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    inquiryAttemptsToday :: Kernel.Prelude.Int,
    inquiryQuotaDate :: Kernel.Prelude.Maybe Data.Time.Day,
    nextInquiryAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    submittedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutBatchListRes = PayoutBatchListRes {batches :: [PayoutBatchListItem], summary :: Dashboard.Common.Summary, totalItems :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutBatchOrdersRes = PayoutBatchOrdersRes {orders :: [PayoutOrderListItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutFlagReason
  = ExceededMaxReferral
  | MinRideDistanceInvalid
  | MinPickupDistanceInvalid
  | CustomerExistAsDriver
  | MultipleDeviceIdExists
  | RideConstraintInvalid
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutOrderListItem = PayoutOrderListItem
  { orderId :: Kernel.Prelude.Text,
    payoutRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerId :: Kernel.Prelude.Text,
    beneficiaryName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryRole :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    transferStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    failureCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementRefType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutReferralHistoryRes = PayoutReferralHistoryRes {history :: [ReferralHistoryItem], summary :: Dashboard.Common.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutRunSummary = PayoutRunSummary
  { evaluatedCount :: Kernel.Prelude.Int,
    excludedCount :: Kernel.Prelude.Int,
    includedCount :: Kernel.Prelude.Int,
    paidCount :: Kernel.Prelude.Int,
    failedCount :: Kernel.Prelude.Int,
    pendingCount :: Kernel.Prelude.Int,
    totalAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    paidAmount :: Kernel.Types.Common.HighPrecMoney,
    failedAmount :: Kernel.Types.Common.HighPrecMoney
  }
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

type API = ("payout" :> (GetPayoutPayoutHistoryHelper :<|> GetPayoutPayoutReferralHistory :<|> GetPayoutPayoutOrder :<|> GetPayoutPayoutHelper :<|> PostPayoutPayoutRetryHelper :<|> PostPayoutPayoutCancelHelper :<|> PostPayoutPayoutCashHelper :<|> PostPayoutPayoutVpaDeleteHelper :<|> PostPayoutPayoutVpaUpdateHelper :<|> PostPayoutPayoutVpaRefundRegistrationHelper :<|> PostPayoutPayoutScheduledPayoutConfigUpsert :<|> GetPayoutPayoutAdhocEligibility :<|> PostPayoutPayoutAdhocInitiate :<|> GetPayoutPayoutBatchList :<|> GetPayoutPayoutBatchOrders))

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

type GetPayoutPayoutHistoryHelper =
  ( "payout" :> "history" :> QueryParam "driverId" Kernel.Prelude.Text :> QueryParam "driverPhoneNo" Kernel.Prelude.Text
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
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
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
           "driverPhoneCountryCode"
           Kernel.Prelude.Text
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

type GetPayoutPayoutOrder = ("payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> Get '[JSON] Lib.Payment.API.Payout.Types.PayoutOrderResp)

type GetPayoutPayout = ("payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> Get '[JSON] Lib.Payment.API.Payout.Types.PayoutRequestResp)

type GetPayoutPayoutHelper =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest)
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get '[JSON] Lib.Payment.API.Payout.Types.PayoutRequestResp
  )

type PostPayoutPayoutRetry =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "retry"
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutRetryHelper =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "retry"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCancel =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cancel"
      :> ReqBody
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutCancelReq
      :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCancelHelper =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cancel"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.PayoutCancelReq
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCash =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cash"
      :> ReqBody
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutCashUpdateReq
      :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutCashHelper =
  ( "payout" :> Capture "payoutRequestId" (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest) :> "cash"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.PayoutCashUpdateReq
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutVpaDelete = ("payout" :> "vpa" :> "delete" :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.DeleteVpaReq :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess)

type PostPayoutPayoutVpaDeleteHelper =
  ( "payout" :> "vpa" :> "delete" :> QueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.DeleteVpaReq
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutVpaUpdate = ("payout" :> "vpa" :> "update" :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.UpdateVpaReq :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess)

type PostPayoutPayoutVpaUpdateHelper =
  ( "payout" :> "vpa" :> "update" :> QueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.UpdateVpaReq
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutVpaRefundRegistration =
  ( "payout" :> "vpa" :> "refundRegistration" :> ReqBody '[JSON] Lib.Payment.API.Payout.Types.RefundRegAmountReq
      :> Post
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutVpaRefundRegistrationHelper =
  ( "payout" :> "vpa" :> "refundRegistration" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Lib.Payment.API.Payout.Types.RefundRegAmountReq
      :> Post '[JSON] Lib.Payment.API.Payout.Types.PayoutSuccess
  )

type PostPayoutPayoutScheduledPayoutConfigUpsert =
  ( "payout" :> "scheduledPayoutConfig" :> "upsert" :> ReqBody '[JSON] UpdateScheduledPayoutConfigReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetPayoutPayoutAdhocEligibility = ("payout" :> "adhoc" :> "eligibility" :> MandatoryQueryParam "personId" Kernel.Prelude.Text :> Get '[JSON] AdhocPayoutEligibilityResp)

type PostPayoutPayoutAdhocInitiate = ("payout" :> "adhoc" :> "initiate" :> ReqBody '[JSON] AdhocPayoutInitiateReq :> Post '[JSON] AdhocPayoutInitiateResp)

type GetPayoutPayoutBatchList =
  ( "payout" :> "batch" :> "list" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "origin" Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin
      :> QueryParam
           "payoutRail"
           Kernel.Prelude.Text
      :> QueryParam
           "status"
           Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           PayoutBatchListRes
  )

type GetPayoutPayoutBatchOrders =
  ( "payout" :> "batch" :> Capture "batchId" Kernel.Prelude.Text :> "orders" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get '[JSON] PayoutBatchOrdersRes
  )

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutHistoryRes,
    getPayoutPayoutReferralHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient PayoutReferralHistoryRes,
    getPayoutPayoutOrder :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutOrderResp,
    getPayoutPayout :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutRequestResp,
    postPayoutPayoutRetry :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutCancel :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutCash :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaDelete :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.DeleteVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaUpdate :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.UpdateVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaRefundRegistration :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.RefundRegAmountReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutScheduledPayoutConfigUpsert :: UpdateScheduledPayoutConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPayoutPayoutAdhocEligibility :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient AdhocPayoutEligibilityResp,
    postPayoutPayoutAdhocInitiate :: AdhocPayoutInitiateReq -> EulerHS.Types.EulerClient AdhocPayoutInitiateResp,
    getPayoutPayoutBatchList :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient PayoutBatchListRes,
    getPayoutPayoutBatchOrders :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient PayoutBatchOrdersRes
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutHistory :<|> getPayoutPayoutReferralHistory :<|> getPayoutPayoutOrder :<|> getPayoutPayout :<|> postPayoutPayoutRetry :<|> postPayoutPayoutCancel :<|> postPayoutPayoutCash :<|> postPayoutPayoutVpaDelete :<|> postPayoutPayoutVpaUpdate :<|> postPayoutPayoutVpaRefundRegistration :<|> postPayoutPayoutScheduledPayoutConfigUpsert :<|> getPayoutPayoutAdhocEligibility :<|> postPayoutPayoutAdhocInitiate :<|> getPayoutPayoutBatchList :<|> getPayoutPayoutBatchOrders = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_HISTORY
  | GET_PAYOUT_PAYOUT_REFERRAL_HISTORY
  | GET_PAYOUT_PAYOUT_ORDER
  | GET_PAYOUT_PAYOUT
  | POST_PAYOUT_PAYOUT_RETRY
  | POST_PAYOUT_PAYOUT_CANCEL
  | POST_PAYOUT_PAYOUT_CASH
  | POST_PAYOUT_PAYOUT_VPA_DELETE
  | POST_PAYOUT_PAYOUT_VPA_UPDATE
  | POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION
  | POST_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG_UPSERT
  | GET_PAYOUT_PAYOUT_ADHOC_ELIGIBILITY
  | POST_PAYOUT_PAYOUT_ADHOC_INITIATE
  | GET_PAYOUT_PAYOUT_BATCH_LIST
  | GET_PAYOUT_PAYOUT_BATCH_ORDERS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PayoutUserActionType])
