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
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import Servant
import Servant.Client

data BankVerifyReq = BankVerifyReq {bankVerifiedAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ManualConfirmReq = ManualConfirmReq {confirmedBy :: Kernel.Prelude.Text, reason :: Kernel.Prelude.Text, confirmedAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OrderRow = OrderRow
  { rsoIds :: [(Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder)],
    orderId :: Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platformGrossFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    receivedTotal :: Kernel.Types.Common.HighPrecMoney,
    orderVerdict :: Lib.Finance.Domain.Types.ReconSettlementOrder.OrderReconVerdict,
    orderDiff :: Kernel.Types.Common.HighPrecMoney,
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
  { rsoIds :: [(Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder)],
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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

data SettlementBatchListRes = SettlementBatchListRes {totalItems :: Kernel.Prelude.Int, batches :: [SettlementBatchSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementBatchOrderListRes = SettlementBatchOrderListRes {totalItems :: Kernel.Prelude.Int, orders :: [OrderRow]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementBatchSummary = SettlementBatchSummary
  { settlementId :: Kernel.Prelude.Text,
    bapId :: Kernel.Prelude.Text,
    receivedAt :: Kernel.Prelude.UTCTime,
    utrCount :: Kernel.Prelude.Int,
    orderCount :: Kernel.Prelude.Int,
    unsentOrderCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementBatchUtrListRes = SettlementBatchUtrListRes {utrs :: [UtrSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UtrDetailRes = UtrDetailRes {utr :: UtrSummary, orders :: [OrderRow]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UtrListRes = UtrListRes {totalItems :: Kernel.Prelude.Int, utrs :: [UtrSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UtrSummary = UtrSummary
  { id :: (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement),
    utr :: Kernel.Prelude.Text,
    bapId :: Kernel.Prelude.Text,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    bankVerifiedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalOrders :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rSFReconciliation" :> (GetRSFReconciliationRsfSettlements :<|> GetRSFReconciliationRsfSettlementsUtrs :<|> GetRSFReconciliationRsfSettlementsOrders :<|> PostRSFReconciliationRsfSettlementsSend :<|> GetRSFReconciliationRsfUtrs :<|> GetRSFReconciliationRsfUtr :<|> PostRSFReconciliationRsfUtrBankVerify :<|> PostRSFReconciliationRsfOrdersConfirm :<|> GetRSFReconciliationRsfReconGrid :<|> GetRSFReconciliationRsfReconUnmatched))

type GetRSFReconciliationRsfSettlements =
  ( "rsf" :> "settlements" :> QueryParam "bapId" Kernel.Prelude.Text :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           SettlementBatchListRes
  )

type GetRSFReconciliationRsfSettlementsUtrs = ("rsf" :> "settlements" :> Capture "settlementId" Kernel.Prelude.Text :> "utrs" :> Get ('[JSON]) SettlementBatchUtrListRes)

type GetRSFReconciliationRsfSettlementsOrders =
  ( "rsf" :> "settlements" :> Capture "settlementId" Kernel.Prelude.Text :> "orders" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) SettlementBatchOrderListRes
  )

type PostRSFReconciliationRsfSettlementsSend = ("rsf" :> "settlements" :> Capture "settlementId" Kernel.Prelude.Text :> "send" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

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

type GetRSFReconciliationRsfUtr = ("rsf" :> "utrs" :> Capture "utrId" ((Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement)) :> Get ('[JSON]) UtrDetailRes)

type PostRSFReconciliationRsfUtrBankVerify =
  ( "rsf" :> "utrs" :> Capture "utrId" ((Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement)) :> "verify"
      :> ReqBody
           ('[JSON])
           BankVerifyReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostRSFReconciliationRsfOrdersConfirm =
  ( "rsf" :> "orders"
      :> Capture
           "rsoId"
           ((Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder))
      :> "confirm"
      :> ReqBody ('[JSON]) ManualConfirmReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
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
  { getRSFReconciliationRsfSettlements :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient SettlementBatchListRes),
    getRSFReconciliationRsfSettlementsUtrs :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient SettlementBatchUtrListRes),
    getRSFReconciliationRsfSettlementsOrders :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient SettlementBatchOrderListRes),
    postRSFReconciliationRsfSettlementsSend :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getRSFReconciliationRsfUtrs :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient UtrListRes),
    getRSFReconciliationRsfUtr :: ((Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement) -> EulerHS.Types.EulerClient UtrDetailRes),
    postRSFReconciliationRsfUtrBankVerify :: ((Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement) -> BankVerifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRSFReconciliationRsfOrdersConfirm :: ((Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder) -> ManualConfirmReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getRSFReconciliationRsfReconGrid :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (ReconTabStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ReconGridListRes),
    getRSFReconciliationRsfReconUnmatched :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ReconGridListRes)
  }

mkRSFReconciliationAPIs :: (Client EulerHS.Types.EulerClient API -> RSFReconciliationAPIs)
mkRSFReconciliationAPIs rSFReconciliationClient = (RSFReconciliationAPIs {..})
  where
    getRSFReconciliationRsfSettlements :<|> getRSFReconciliationRsfSettlementsUtrs :<|> getRSFReconciliationRsfSettlementsOrders :<|> postRSFReconciliationRsfSettlementsSend :<|> getRSFReconciliationRsfUtrs :<|> getRSFReconciliationRsfUtr :<|> postRSFReconciliationRsfUtrBankVerify :<|> postRSFReconciliationRsfOrdersConfirm :<|> getRSFReconciliationRsfReconGrid :<|> getRSFReconciliationRsfReconUnmatched = rSFReconciliationClient

data RSFReconciliationUserActionType
  = GET_RSF_RECONCILIATION_RSF_SETTLEMENTS
  | GET_RSF_RECONCILIATION_RSF_SETTLEMENTS_UTRS
  | GET_RSF_RECONCILIATION_RSF_SETTLEMENTS_ORDERS
  | POST_RSF_RECONCILIATION_RSF_SETTLEMENTS_SEND
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
