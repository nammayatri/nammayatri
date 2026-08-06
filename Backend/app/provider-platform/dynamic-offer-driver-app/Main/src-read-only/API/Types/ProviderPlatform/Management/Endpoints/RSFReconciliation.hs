{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry
import Servant
import Servant.Client

data BankVerifyReq = BankVerifyReq {bankVerifiedAmount :: Kernel.Types.Common.HighPrecMoney, verifiedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ManualConfirmReq = ManualConfirmReq {utr :: Kernel.Prelude.Text, confirmedBy :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Text, confirmedAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageBatchListRes = MessageBatchListRes {totalItems :: Kernel.Prelude.Int, batches :: [MessageBatchSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageBatchOrderListRes = MessageBatchOrderListRes {totalItems :: Kernel.Prelude.Int, orders :: [OrderRow]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageBatchSummary = MessageBatchSummary
  { messageId :: Kernel.Prelude.Text,
    bapId :: Kernel.Prelude.Text,
    receivedAt :: Kernel.Prelude.UTCTime,
    utrCount :: Kernel.Prelude.Int,
    orderCount :: Kernel.Prelude.Int,
    processed :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageBatchUtrListRes = MessageBatchUtrListRes {utrs :: [UtrSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OrderReconVerdict
  = AWAITING
  | PAID
  | UNDERPAID
  | OVERPAID
  | REJECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OrderRow = OrderRow
  { orderId :: Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platformGrossFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    receivedTotal :: Kernel.Types.Common.HighPrecMoney,
    orderVerdict :: OrderReconVerdict,
    orderDiff :: Kernel.Types.Common.HighPrecMoney,
    claimStatus :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfClaimStatus,
    settlementUtrs :: [Kernel.Prelude.Text],
    anyManuallyConfirmed :: Kernel.Prelude.Bool,
    allSent :: Kernel.Prelude.Bool,
    receivedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconGridListRes = ReconGridListRes {totalItems :: Kernel.Prelude.Int, rows :: [ReconGridRow]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconGridRow = ReconGridRow
  { rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    buyerAppName :: Kernel.Prelude.Text,
    rideDateTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    grossFarePlatform :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    netReceivablePlatform :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bapSettlementAmount :: Kernel.Types.Common.HighPrecMoney,
    amountDifference :: Kernel.Types.Common.HighPrecMoney,
    settlementDateBap :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementUtrs :: [Kernel.Prelude.Text],
    reconciliationStatus :: ReconTabStatus,
    payoutEligible :: Kernel.Prelude.Bool,
    anyManuallyConfirmed :: Kernel.Prelude.Bool,
    communicationStatus :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconTabStatus
  = Matched
  | Unmatched
  | Mismatch
  | Pending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data UtrDetailRes = UtrDetailRes {utr :: UtrSummary, orders :: [OrderRow]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UtrListRes = UtrListRes {totalItems :: Kernel.Prelude.Int, utrs :: [UtrSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UtrSummary = UtrSummary
  { utr :: Kernel.Prelude.Text,
    bapId :: Kernel.Prelude.Text,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    bankVerifiedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    unallocatedAmount :: Kernel.Types.Common.HighPrecMoney,
    totalOrders :: Kernel.Prelude.Int,
    reportedStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rSFReconciliation" :> (GetRSFReconciliationRsfMessages :<|> GetRSFReconciliationRsfMessagesUtrs :<|> GetRSFReconciliationRsfMessagesOrders :<|> PostRSFReconciliationRsfMessagesSend :<|> GetRSFReconciliationRsfUtrs :<|> GetRSFReconciliationRsfUtr :<|> PostRSFReconciliationRsfUtrBankVerify :<|> PostRSFReconciliationRsfOrdersConfirm :<|> GetRSFReconciliationRsfReconGrid :<|> GetRSFReconciliationRsfReconUnmatched))

type GetRSFReconciliationRsfMessages =
  ( "rsf" :> "messages" :> QueryParam "bapId" Kernel.Prelude.Text :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get ('[JSON]) MessageBatchListRes
  )

type GetRSFReconciliationRsfMessagesUtrs = ("rsf" :> "messages" :> Capture "messageId" Kernel.Prelude.Text :> "utrs" :> Get ('[JSON]) MessageBatchUtrListRes)

type GetRSFReconciliationRsfMessagesOrders =
  ( "rsf" :> "messages" :> Capture "messageId" Kernel.Prelude.Text :> "orders" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) MessageBatchOrderListRes
  )

type PostRSFReconciliationRsfMessagesSend = ("rsf" :> "messages" :> Capture "messageId" Kernel.Prelude.Text :> "send" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type GetRSFReconciliationRsfUtrs =
  ( "rsf" :> "utrs" :> QueryParam "bapId" Kernel.Prelude.Text :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "isVerified"
           Kernel.Prelude.Bool
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           UtrListRes
  )

type GetRSFReconciliationRsfUtr = ("rsf" :> "utrs" :> Capture "utr" Kernel.Prelude.Text :> Get ('[JSON]) UtrDetailRes)

type PostRSFReconciliationRsfUtrBankVerify = ("rsf" :> "utrs" :> Capture "utr" Kernel.Prelude.Text :> "verify" :> ReqBody ('[JSON]) BankVerifyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostRSFReconciliationRsfOrdersConfirm =
  ( "rsf" :> "orders" :> Capture "orderId" Kernel.Prelude.Text :> "confirm" :> ReqBody ('[JSON]) ManualConfirmReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetRSFReconciliationRsfReconGrid =
  ( "rsf" :> "recon" :> "grid" :> QueryParam "bapId" Kernel.Prelude.Text :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "manuallyConfirmedOnly" Kernel.Prelude.Bool
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           ReconTabStatus
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           ReconGridListRes
  )

type GetRSFReconciliationRsfReconUnmatched =
  ( "rsf" :> "recon" :> "unmatched" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get ('[JSON]) ReconGridListRes
  )

data RSFReconciliationAPIs = RSFReconciliationAPIs
  { getRSFReconciliationRsfMessages :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient MessageBatchListRes),
    getRSFReconciliationRsfMessagesUtrs :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient MessageBatchUtrListRes),
    getRSFReconciliationRsfMessagesOrders :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient MessageBatchOrderListRes),
    postRSFReconciliationRsfMessagesSend :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getRSFReconciliationRsfUtrs :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient UtrListRes),
    getRSFReconciliationRsfUtr :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient UtrDetailRes),
    postRSFReconciliationRsfUtrBankVerify :: (Kernel.Prelude.Text -> BankVerifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRSFReconciliationRsfOrdersConfirm :: (Kernel.Prelude.Text -> ManualConfirmReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getRSFReconciliationRsfReconGrid :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (ReconTabStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ReconGridListRes),
    getRSFReconciliationRsfReconUnmatched :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ReconGridListRes)
  }

mkRSFReconciliationAPIs :: (Client EulerHS.Types.EulerClient API -> RSFReconciliationAPIs)
mkRSFReconciliationAPIs rSFReconciliationClient = (RSFReconciliationAPIs {..})
  where
    getRSFReconciliationRsfMessages :<|> getRSFReconciliationRsfMessagesUtrs :<|> getRSFReconciliationRsfMessagesOrders :<|> postRSFReconciliationRsfMessagesSend :<|> getRSFReconciliationRsfUtrs :<|> getRSFReconciliationRsfUtr :<|> postRSFReconciliationRsfUtrBankVerify :<|> postRSFReconciliationRsfOrdersConfirm :<|> getRSFReconciliationRsfReconGrid :<|> getRSFReconciliationRsfReconUnmatched = rSFReconciliationClient

data RSFReconciliationUserActionType
  = GET_RSF_RECONCILIATION_RSF_MESSAGES
  | GET_RSF_RECONCILIATION_RSF_MESSAGES_UTRS
  | GET_RSF_RECONCILIATION_RSF_MESSAGES_ORDERS
  | POST_RSF_RECONCILIATION_RSF_MESSAGES_SEND
  | GET_RSF_RECONCILIATION_RSF_UTRS
  | GET_RSF_RECONCILIATION_RSF_UTR
  | POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY
  | POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM
  | GET_RSF_RECONCILIATION_RSF_RECON_GRID
  | GET_RSF_RECONCILIATION_RSF_RECON_UNMATCHED
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''ReconTabStatus))

$(Data.Singletons.TH.genSingletons [(''RSFReconciliationUserActionType)])
