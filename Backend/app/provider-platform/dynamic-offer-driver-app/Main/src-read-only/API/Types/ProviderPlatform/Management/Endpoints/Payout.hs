{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Payout where

import qualified Dashboard.Common
import Data.Aeson
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
import Kernel.Utils.TH
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.Common
import qualified "payment" Lib.Payment.Domain.Types.PayoutBatch
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Servant.Client

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

data AdhocPayoutLookupResp = AdhocPayoutLookupResp
  { personId :: Kernel.Prelude.Text,
    personName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    role :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    walletBalance :: Kernel.Types.Common.HighPrecMoney,
    nonRedeemableAmount :: Kernel.Types.Common.HighPrecMoney,
    payoutableBalance :: Kernel.Types.Common.HighPrecMoney,
    minimumPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    isEligible :: Kernel.Prelude.Bool,
    ineligibilityReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutServiceFlow :: Kernel.Prelude.Text,
    bankAccountStatus :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
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
    origin :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin,
    status :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus,
    payoutRail :: Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail,
    executionDate :: Data.Time.Day,
    clientRefNo :: Kernel.Prelude.Text,
    partnerBatchRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    itemCount :: Kernel.Prelude.Int,
    totalAmount :: Kernel.Types.Common.HighPrecMoney,
    excludedCount :: Kernel.Prelude.Int,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    statusCheckRound :: Kernel.Prelude.Int,
    nextStatusCallAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    statusNoDataReplies :: Kernel.Prelude.Int,
    submittedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutBatchListRes = PayoutBatchListRes {batches :: [PayoutBatchListItem], hasMore :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutBatchOrdersRes = PayoutBatchOrdersRes {orders :: [PayoutOrderListItem], excluded :: [PayoutExcludedItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutConfigCommand
  = VIEW
  | VIEW_DIFF
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PayoutExcludedItem = PayoutExcludedItem
  { personId :: Kernel.Prelude.Text,
    personName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    role :: Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    reason :: Kernel.Prelude.Text,
    excludedAt :: Kernel.Prelude.UTCTime,
    payoutRequestId :: Kernel.Prelude.Text,
    batchId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutExcludedRes = PayoutExcludedRes {items :: [PayoutExcludedItem], hasMore :: Kernel.Prelude.Bool}
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
    shortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerId :: Kernel.Prelude.Text,
    beneficiaryName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryRole :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    transferStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    responseMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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

data ScheduledPayoutConfigAPIEntity = ScheduledPayoutConfigAPIEntity
  { payoutCategory :: Lib.Payment.Domain.Types.Common.EntityName,
    isEnabled :: Kernel.Prelude.Bool,
    frequency :: ScheduledPayoutFrequency,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dayOfMonth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeOfDay :: Kernel.Prelude.Text,
    intervalHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    intervalDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    batchSize :: Kernel.Prelude.Int,
    itemsPerBatchLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minimumPayoutAmount :: Kernel.Types.Common.HighPrecMoney,
    maxRetriesPerDriver :: Kernel.Prelude.Int,
    defaultPayoutRail :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    rescheduleBufferMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bufferCheckEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    bulkStatusCheckIntervalMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bulkStatusCheckBatchLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    orderType :: Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledPayoutConfigFieldDiff = ScheduledPayoutConfigFieldDiff {field :: Kernel.Prelude.Text, from :: Kernel.Prelude.Maybe Kernel.Prelude.Text, to :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledPayoutConfigViewResp = ScheduledPayoutConfigViewResp
  { configs :: [ScheduledPayoutConfigAPIEntity],
    diff :: Kernel.Prelude.Maybe [ScheduledPayoutConfigFieldDiff],
    scheduleEffect :: Kernel.Prelude.Maybe ScheduledPayoutScheduleEffect
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledPayoutFrequency
  = DAILY
  | WEEKLY
  | MONTHLY
  | HOURLY
  | EVERY_N_DAYS
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data ScheduledPayoutRescheduleAction
  = MOVE_JOB
  | LEAVE_INSIDE_BUFFER
  | NO_QUEUED_JOB
  | NOT_MOVED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledPayoutScheduleEffect = ScheduledPayoutScheduleEffect
  { currentNextRunAt :: Kernel.Prelude.UTCTime,
    proposedNextRunAt :: Kernel.Prelude.UTCTime,
    queuedJobAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    action :: ScheduledPayoutRescheduleAction,
    bufferMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
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
    timeDiffFromUtc :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    intervalHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    intervalDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rescheduleBufferMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bufferCheckEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    itemsPerBatchLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultPayoutRail :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail,
    bulkStatusCheckIntervalMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bulkStatusCheckBatchLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("payout" :> (GetPayoutPayoutHistoryHelper :<|> GetPayoutPayoutReferralHistoryHelper :<|> GetPayoutPayoutOrderHelper :<|> GetPayoutPayoutScheduledPayoutConfigHelper :<|> GetPayoutPayoutHelper :<|> PostPayoutPayoutRetryHelper :<|> PostPayoutPayoutCancelHelper :<|> PostPayoutPayoutCashHelper :<|> PostPayoutPayoutVpaDeleteHelper :<|> PostPayoutPayoutVpaUpdateHelper :<|> PostPayoutPayoutVpaRefundRegistrationHelper :<|> PostPayoutPayoutScheduledPayoutConfigUpsertHelper :<|> GetPayoutAdhocLookupHelper :<|> PostPayoutAdhocInitiateHelper :<|> GetPayoutBatchListHelper :<|> GetPayoutBatchOrdersHelper :<|> GetPayoutExcludedHelper))

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

type GetPayoutPayoutReferralHistoryHelper =
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
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           PayoutReferralHistoryRes
  )

type GetPayoutPayoutOrder = ("payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> Get '[JSON] Lib.Payment.API.Payout.Types.PayoutOrderResp)

type GetPayoutPayoutOrderHelper =
  ( "payout" :> "order" :> Capture "payoutOrderId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           Lib.Payment.API.Payout.Types.PayoutOrderResp
  )

type GetPayoutPayoutScheduledPayoutConfig =
  ( "payout" :> "scheduledPayoutConfig" :> QueryParam "batchSize" Kernel.Prelude.Int
      :> QueryParam
           "bufferCheckEnabled"
           Kernel.Prelude.Bool
      :> QueryParam "command" Kernel.Prelude.Text
      :> QueryParam "dayOfMonth" Kernel.Prelude.Int
      :> QueryParam
           "dayOfWeek"
           Kernel.Prelude.Int
      :> QueryParam
           "frequency"
           Kernel.Prelude.Text
      :> QueryParam
           "intervalDays"
           Kernel.Prelude.Int
      :> QueryParam
           "intervalHours"
           Kernel.Prelude.Int
      :> QueryParam
           "isEnabled"
           Kernel.Prelude.Bool
      :> QueryParam
           "minimumPayoutAmount"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "payoutCategory"
           Kernel.Prelude.Text
      :> QueryParam
           "rescheduleBufferMinutes"
           Kernel.Prelude.Int
      :> QueryParam
           "timeOfDay"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           ScheduledPayoutConfigViewResp
  )

type GetPayoutPayoutScheduledPayoutConfigHelper =
  ( "payout" :> "scheduledPayoutConfig" :> QueryParam "batchSize" Kernel.Prelude.Int
      :> QueryParam
           "bufferCheckEnabled"
           Kernel.Prelude.Bool
      :> QueryParam "command" Kernel.Prelude.Text
      :> QueryParam "dayOfMonth" Kernel.Prelude.Int
      :> QueryParam
           "dayOfWeek"
           Kernel.Prelude.Int
      :> QueryParam
           "frequency"
           Kernel.Prelude.Text
      :> QueryParam
           "intervalDays"
           Kernel.Prelude.Int
      :> QueryParam
           "intervalHours"
           Kernel.Prelude.Int
      :> QueryParam
           "isEnabled"
           Kernel.Prelude.Bool
      :> QueryParam
           "minimumPayoutAmount"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "payoutCategory"
           Kernel.Prelude.Text
      :> QueryParam
           "rescheduleBufferMinutes"
           Kernel.Prelude.Int
      :> QueryParam
           "timeOfDay"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           ScheduledPayoutConfigViewResp
  )

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

type PostPayoutPayoutScheduledPayoutConfigUpsertHelper =
  ( "payout" :> "scheduledPayoutConfig" :> "upsert" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           UpdateScheduledPayoutConfigReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetPayoutAdhocLookup = ("adhoc" :> "lookup" :> MandatoryQueryParam "personId" Kernel.Prelude.Text :> Get '[JSON] AdhocPayoutLookupResp)

type GetPayoutAdhocLookupHelper = ("adhoc" :> "lookup" :> QueryParam "requestorId" Kernel.Prelude.Text :> MandatoryQueryParam "personId" Kernel.Prelude.Text :> Get '[JSON] AdhocPayoutLookupResp)

type PostPayoutAdhocInitiate = ("adhoc" :> "initiate" :> ReqBody '[JSON] AdhocPayoutInitiateReq :> Post '[JSON] AdhocPayoutInitiateResp)

type PostPayoutAdhocInitiateHelper = ("adhoc" :> "initiate" :> QueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] AdhocPayoutInitiateReq :> Post '[JSON] AdhocPayoutInitiateResp)

type GetPayoutBatchList =
  ( "batch" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "origin"
           Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin
      :> QueryParam "payoutRail" Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail
      :> QueryParam
           "status"
           Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus
      :> MandatoryQueryParam
           "fromDate"
           Data.Time.Day
      :> MandatoryQueryParam
           "toDate"
           Data.Time.Day
      :> Get
           '[JSON]
           PayoutBatchListRes
  )

type GetPayoutBatchListHelper =
  ( "batch" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "origin"
           Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin
      :> QueryParam
           "payoutRail"
           Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail
      :> QueryParam
           "status"
           Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "fromDate"
           Data.Time.Day
      :> MandatoryQueryParam
           "toDate"
           Data.Time.Day
      :> Get
           '[JSON]
           PayoutBatchListRes
  )

type GetPayoutBatchOrders = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> "orders" :> Get '[JSON] PayoutBatchOrdersRes)

type GetPayoutBatchOrdersHelper = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> "orders" :> QueryParam "requestorId" Kernel.Prelude.Text :> Get '[JSON] PayoutBatchOrdersRes)

type GetPayoutExcluded =
  ( "excluded" :> QueryParam "from" Data.Time.Day :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "to"
           Data.Time.Day
      :> Get '[JSON] PayoutExcludedRes
  )

type GetPayoutExcludedHelper =
  ( "excluded" :> QueryParam "from" Data.Time.Day :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "to"
           Data.Time.Day
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get '[JSON] PayoutExcludedRes
  )

data PayoutAPIs = PayoutAPIs
  { getPayoutPayoutHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutHistoryRes,
    getPayoutPayoutReferralHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient PayoutReferralHistoryRes,
    getPayoutPayoutOrder :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutOrderResp,
    getPayoutPayoutScheduledPayoutConfig :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient ScheduledPayoutConfigViewResp,
    getPayoutPayout :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutRequestResp,
    postPayoutPayoutRetry :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutCancel :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutCash :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaDelete :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.DeleteVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaUpdate :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.UpdateVpaReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutVpaRefundRegistration :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Payment.API.Payout.Types.RefundRegAmountReq -> EulerHS.Types.EulerClient Lib.Payment.API.Payout.Types.PayoutSuccess,
    postPayoutPayoutScheduledPayoutConfigUpsert :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> UpdateScheduledPayoutConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPayoutAdhocLookup :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient AdhocPayoutLookupResp,
    postPayoutAdhocInitiate :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> AdhocPayoutInitiateReq -> EulerHS.Types.EulerClient AdhocPayoutInitiateResp,
    getPayoutBatchList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchOrigin -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchRail -> Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Time.Day -> Data.Time.Day -> EulerHS.Types.EulerClient PayoutBatchListRes,
    getPayoutBatchOrders :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient PayoutBatchOrdersRes,
    getPayoutExcluded :: Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient PayoutExcludedRes
  }

mkPayoutAPIs :: (Client EulerHS.Types.EulerClient API -> PayoutAPIs)
mkPayoutAPIs payoutClient = (PayoutAPIs {..})
  where
    getPayoutPayoutHistory :<|> getPayoutPayoutReferralHistory :<|> getPayoutPayoutOrder :<|> getPayoutPayoutScheduledPayoutConfig :<|> getPayoutPayout :<|> postPayoutPayoutRetry :<|> postPayoutPayoutCancel :<|> postPayoutPayoutCash :<|> postPayoutPayoutVpaDelete :<|> postPayoutPayoutVpaUpdate :<|> postPayoutPayoutVpaRefundRegistration :<|> postPayoutPayoutScheduledPayoutConfigUpsert :<|> getPayoutAdhocLookup :<|> postPayoutAdhocInitiate :<|> getPayoutBatchList :<|> getPayoutBatchOrders :<|> getPayoutExcluded = payoutClient

data PayoutUserActionType
  = GET_PAYOUT_PAYOUT_HISTORY
  | GET_PAYOUT_PAYOUT_REFERRAL_HISTORY
  | GET_PAYOUT_PAYOUT_ORDER
  | GET_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG
  | GET_PAYOUT_PAYOUT
  | POST_PAYOUT_PAYOUT_RETRY
  | POST_PAYOUT_PAYOUT_CANCEL
  | POST_PAYOUT_PAYOUT_CASH
  | POST_PAYOUT_PAYOUT_VPA_DELETE
  | POST_PAYOUT_PAYOUT_VPA_UPDATE
  | POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION
  | POST_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG_UPSERT
  | GET_PAYOUT_ADHOC_LOOKUP
  | POST_PAYOUT_ADHOC_INITIATE
  | GET_PAYOUT_BATCH_LIST
  | GET_PAYOUT_BATCH_ORDERS
  | GET_PAYOUT_EXCLUDED
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PayoutConfigCommand)

$(mkHttpInstancesForEnum ''ScheduledPayoutFrequency)

$(Data.Singletons.TH.genSingletons [''PayoutUserActionType])
