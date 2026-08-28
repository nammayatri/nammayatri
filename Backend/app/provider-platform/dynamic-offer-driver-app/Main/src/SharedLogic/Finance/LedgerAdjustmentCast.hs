module SharedLogic.Finance.LedgerAdjustmentCast
  ( castAdjustmentCategory,
    toApiAdjustmentCategory,
    castAdjustmentDirection,
    toApiAdjustmentDirection,
    castAdjustmentRequestStatus,
    toApiAdjustmentRequestStatus,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement as API
import qualified Domain.Types.LedgerAdjustmentRequest as DLA

castAdjustmentCategory :: API.AdjustmentCategory -> DLA.AdjustmentCategory
castAdjustmentCategory = \case
  API.RideRelatedCredit -> DLA.RideRelatedCredit
  API.RideRelatedDebit -> DLA.RideRelatedDebit
  API.PayoutRelatedCredit -> DLA.PayoutRelatedCredit
  API.PayoutRelatedDebit -> DLA.PayoutRelatedDebit
  API.TdsReimbursementCredit -> DLA.TdsReimbursementCredit
  API.TdsReimbursementDebit -> DLA.TdsReimbursementDebit
  API.IncentiveCredit -> DLA.IncentiveCredit
  API.IncentiveDebit -> DLA.IncentiveDebit
  API.MiscellaneousCredit -> DLA.MiscellaneousCredit
  API.MiscellaneousDebit -> DLA.MiscellaneousDebit
  API.TdsDeductionDebit -> DLA.TdsDeductionDebit

toApiAdjustmentCategory :: DLA.AdjustmentCategory -> API.AdjustmentCategory
toApiAdjustmentCategory = \case
  DLA.RideRelatedCredit -> API.RideRelatedCredit
  DLA.RideRelatedDebit -> API.RideRelatedDebit
  DLA.PayoutRelatedCredit -> API.PayoutRelatedCredit
  DLA.PayoutRelatedDebit -> API.PayoutRelatedDebit
  DLA.TdsReimbursementCredit -> API.TdsReimbursementCredit
  DLA.TdsReimbursementDebit -> API.TdsReimbursementDebit
  DLA.IncentiveCredit -> API.IncentiveCredit
  DLA.IncentiveDebit -> API.IncentiveDebit
  DLA.MiscellaneousCredit -> API.MiscellaneousCredit
  DLA.MiscellaneousDebit -> API.MiscellaneousDebit
  DLA.TdsDeductionDebit -> API.TdsDeductionDebit

castAdjustmentDirection :: API.AdjustmentDirection -> DLA.AdjustmentDirection
castAdjustmentDirection = \case
  API.Credit -> DLA.Credit
  API.Debit -> DLA.Debit

toApiAdjustmentDirection :: DLA.AdjustmentDirection -> API.AdjustmentDirection
toApiAdjustmentDirection = \case
  DLA.Credit -> API.Credit
  DLA.Debit -> API.Debit

castAdjustmentRequestStatus :: API.AdjustmentRequestStatus -> DLA.AdjustmentRequestStatus
castAdjustmentRequestStatus = \case
  API.PENDING_APPROVAL -> DLA.PENDING_APPROVAL
  API.REJECTED -> DLA.REJECTED
  API.POSTED -> DLA.POSTED
  API.POST_FAILED -> DLA.POST_FAILED

toApiAdjustmentRequestStatus :: DLA.AdjustmentRequestStatus -> API.AdjustmentRequestStatus
toApiAdjustmentRequestStatus = \case
  DLA.PENDING_APPROVAL -> API.PENDING_APPROVAL
  DLA.REJECTED -> API.REJECTED
  DLA.POSTED -> API.POSTED
  DLA.POST_FAILED -> API.POST_FAILED
