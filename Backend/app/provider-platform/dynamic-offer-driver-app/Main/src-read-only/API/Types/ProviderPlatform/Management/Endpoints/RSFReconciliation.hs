{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
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

data AllocationLine = AllocationLine {orderId :: Kernel.Prelude.Text, utr :: Kernel.Prelude.Text, amount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AutoAllocationRes = AutoAllocationRes {ordersConsidered :: Kernel.Prelude.Int, ordersSettled :: Kernel.Prelude.Int, ordersPartiallyFunded :: Kernel.Prelude.Int, allocations :: [AllocationLine]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BankVerifyReq = BankVerifyReq {bankVerifiedAmount :: Kernel.Types.Common.HighPrecMoney, verifiedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ManualConfirmReq = ManualConfirmReq {utr :: Kernel.Prelude.Text, confirmedBy :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Text, confirmedAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OrderAllocation = OrderAllocation {utr :: Kernel.Prelude.Text, amount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OrderListRes = OrderListRes {totalItems :: Kernel.Prelude.Int, orders :: [OrderRow]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OrderReconVerdict
  = AWAITING
  | PAID
  | UNDERPAID
  | OVERPAID
  | REJECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data OrderRow = OrderRow
  { orderId :: Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bapId :: Kernel.Prelude.Text,
    platformGrossFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    receivedTotal :: Kernel.Types.Common.HighPrecMoney,
    orderVerdict :: OrderReconVerdict,
    orderDiff :: Kernel.Types.Common.HighPrecMoney,
    claimStatus :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfClaimStatus,
    settlementUtrs :: [Kernel.Prelude.Text],
    allocations :: [OrderAllocation],
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

data SendForDateRes = SendForDateRes {messagesSent :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
    allocatedAmount :: Kernel.Types.Common.HighPrecMoney,
    unallocatedAmount :: Kernel.Types.Common.HighPrecMoney,
    totalOrders :: Kernel.Prelude.Int,
    reportedStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rSFReconciliation" :> (GetRSFReconciliationRsfOrders :<|> GetRSFReconciliationRsfUtrs :<|> GetRSFReconciliationRsfUtr :<|> PostRSFReconciliationRsfUtrBankVerify :<|> PostRSFReconciliationRsfAutoAllocation :<|> PostRSFReconciliationRsfOrdersConfirm :<|> PostRSFReconciliationRsfSend :<|> GetRSFReconciliationRsfReconUnmatched))

type GetRSFReconciliationRsfOrders =
  ( "rsf" :> "orders" :> QueryParam "bapId" Kernel.Prelude.Text :> QueryParam "date" Data.Time.Day :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "status" OrderReconVerdict
      :> QueryParam
           "utr"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           OrderListRes
  )

type GetRSFReconciliationRsfUtrs =
  ( "rsf" :> "utrs" :> QueryParam "bapId" Kernel.Prelude.Text :> QueryParam "date" Data.Time.Day :> QueryParam "isVerified" Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get ('[JSON]) UtrListRes
  )

type GetRSFReconciliationRsfUtr = ("rsf" :> "utrs" :> Capture "utr" Kernel.Prelude.Text :> Get ('[JSON]) UtrDetailRes)

type PostRSFReconciliationRsfUtrBankVerify = ("rsf" :> "utrs" :> Capture "utr" Kernel.Prelude.Text :> "verify" :> ReqBody ('[JSON]) BankVerifyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostRSFReconciliationRsfAutoAllocation = ("rsf" :> "autoAllocation" :> QueryParam "date" Data.Time.Day :> Post ('[JSON]) AutoAllocationRes)

type PostRSFReconciliationRsfOrdersConfirm =
  ( "rsf" :> "orders" :> Capture "orderId" Kernel.Prelude.Text :> "confirm" :> ReqBody ('[JSON]) ManualConfirmReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostRSFReconciliationRsfSend = ("rsf" :> "send" :> QueryParam "date" Data.Time.Day :> Post ('[JSON]) SendForDateRes)

type GetRSFReconciliationRsfReconUnmatched =
  ( "rsf" :> "recon" :> "unmatched" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get ('[JSON]) ReconGridListRes
  )

data RSFReconciliationAPIs = RSFReconciliationAPIs
  { getRSFReconciliationRsfOrders :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (OrderReconVerdict) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient OrderListRes),
    getRSFReconciliationRsfUtrs :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient UtrListRes),
    getRSFReconciliationRsfUtr :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient UtrDetailRes),
    postRSFReconciliationRsfUtrBankVerify :: (Kernel.Prelude.Text -> BankVerifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRSFReconciliationRsfAutoAllocation :: (Kernel.Prelude.Maybe (Data.Time.Day) -> EulerHS.Types.EulerClient AutoAllocationRes),
    postRSFReconciliationRsfOrdersConfirm :: (Kernel.Prelude.Text -> ManualConfirmReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRSFReconciliationRsfSend :: (Kernel.Prelude.Maybe (Data.Time.Day) -> EulerHS.Types.EulerClient SendForDateRes),
    getRSFReconciliationRsfReconUnmatched :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ReconGridListRes)
  }

mkRSFReconciliationAPIs :: (Client EulerHS.Types.EulerClient API -> RSFReconciliationAPIs)
mkRSFReconciliationAPIs rSFReconciliationClient = (RSFReconciliationAPIs {..})
  where
    getRSFReconciliationRsfOrders :<|> getRSFReconciliationRsfUtrs :<|> getRSFReconciliationRsfUtr :<|> postRSFReconciliationRsfUtrBankVerify :<|> postRSFReconciliationRsfAutoAllocation :<|> postRSFReconciliationRsfOrdersConfirm :<|> postRSFReconciliationRsfSend :<|> getRSFReconciliationRsfReconUnmatched = rSFReconciliationClient

data RSFReconciliationUserActionType
  = GET_RSF_RECONCILIATION_RSF_ORDERS
  | GET_RSF_RECONCILIATION_RSF_UTRS
  | GET_RSF_RECONCILIATION_RSF_UTR
  | POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY
  | POST_RSF_RECONCILIATION_RSF_AUTO_ALLOCATION
  | POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM
  | POST_RSF_RECONCILIATION_RSF_SEND
  | GET_RSF_RECONCILIATION_RSF_RECON_UNMATCHED
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''OrderReconVerdict))

$(mkHttpInstancesForEnum (''ReconTabStatus))

$(Data.Singletons.TH.genSingletons [(''RSFReconciliationUserActionType)])
