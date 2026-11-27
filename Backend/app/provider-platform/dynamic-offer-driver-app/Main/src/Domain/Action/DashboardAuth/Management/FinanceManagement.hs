{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for the finance routes of direct-dashboard that act for a fleet. The TDS reimbursement
-- routes treat the requestor as the fleet; the invoice routes take the fleet or driver as a query parameter that
-- nothing otherwise ties to the caller. Once fleet scope is enforced the fleet is the one in the caller's scope
-- (the only one they hold, or, for an invoice, the one named, which has to be in scope, or a driver in one), and
-- it is passed on as the requestor too. Otherwise the dashboard person is the requestor, as before.
module Domain.Action.DashboardAuth.Management.FinanceManagement
  ( getFinanceManagementFinanceInvoiceList,
    getFinanceManagementFinanceInvoicePdf,
    postFinanceManagementTdsReimbursementRequestSubmit,
    getFinanceManagementTdsReimbursementStatus,
  )
where

import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.FinanceManagement
import qualified "beckn-spec" Domain.Types.Invoice
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Lib.Finance.Reconciliation.Types
import Servant
import qualified SharedLogic.FleetScope
import Tools.Auth
import Tools.Auth.DashboardUserAuth

getFinanceManagementFinanceInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Domain.Types.Invoice.IssuedToType -> Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementFinanceInvoiceList a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a12 = do
    subject <- scopedSubject a12 a11
    Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList a14 a13 (Kernel.Prelude.Just subject) a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Domain.Types.Invoice.IssuedToType -> Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.FinanceInvoicePdfResp)
getFinanceManagementFinanceInvoicePdf a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a14 = do
    subject <- scopedSubject a14 a13
    Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoicePdf a16 a15 (Kernel.Prelude.Just subject) a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoicePdf a16 a15 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

postFinanceManagementTdsReimbursementRequestSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRequestSubmitReq -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRequestSubmitRes)
postFinanceManagementTdsReimbursementRequestSubmit a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a2 = do
    (_, requestor) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a2 Kernel.Prelude.Nothing
    Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementTdsReimbursementRequestSubmit a4 a3 requestor a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementTdsReimbursementRequestSubmit a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

getFinanceManagementTdsReimbursementStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementQuarter -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementStatusRes)
getFinanceManagementTdsReimbursementStatus a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a3 = do
    (_, requestor) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId a3 Kernel.Prelude.Nothing
    Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementTdsReimbursementStatus a5 a4 a2 a1 requestor
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementTdsReimbursementStatus a5 a4 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)

-- | The fleet or driver an invoice route is about: the one named, once it is shown to be in the caller's scope, or
-- else the only fleet the caller has.
scopedSubject :: DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text
scopedSubject dashboardUser = \case
  Kernel.Prelude.Just named -> do
    void $ SharedLogic.FleetScope.subjectActingFleet dashboardUser named
    Kernel.Prelude.pure named
  Kernel.Prelude.Nothing -> SharedLogic.FleetScope.scopedFleetOwnerId dashboardUser Kernel.Prelude.Nothing
