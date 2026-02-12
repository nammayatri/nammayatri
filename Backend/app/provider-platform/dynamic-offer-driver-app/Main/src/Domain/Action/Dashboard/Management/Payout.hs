module Domain.Action.Dashboard.Management.Payout
  ( getPayoutPayout,
    postPayoutPayoutRetry,
    postPayoutPayoutCancel,
    postPayoutPayoutCash,
    postPayoutPayoutVpaDelete,
    postPayoutPayoutVpaUpdate,
    postPayoutPayoutVpaRefundRegistration,
  )
where

import qualified Domain.Action.Common.PayoutRequest as CommonPayout
import qualified Domain.Action.Dashboard.PayoutRequest as DashboardPayoutRequest
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as Id
import qualified Lib.Payment.API.Payout as PayoutAPI
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import Servant (ServerT, (:<|>) (..))

payoutServer ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ServerT PayoutAPI.DashboardAPI Environment.Flow
payoutServer merchantShortId opCity =
  PayoutAPI.payoutDashboardHandler
    PayoutAPI.PayoutDashboardHandlerConfig
      { refreshPayoutRequest = CommonPayout.refreshPayoutRequestStatus,
        executePayoutRetry = CommonPayout.executeSpecialZonePayoutRequest,
        handleDeleteVpa = DashboardPayoutRequest.deleteVpa,
        handleUpdateVpa = DashboardPayoutRequest.updateVpa,
        handleRefundRegistrationAmount = DashboardPayoutRequest.refundRegistrationAmount merchantShortId opCity
      }

getPayoutPayout ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  Environment.Flow PayoutTypes.PayoutRequestResp
getPayoutPayout merchantShortId opCity payoutRequestId = do
  let (getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  getById payoutRequestId

postPayoutPayoutRetry ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutRetry merchantShortId opCity payoutRequestId = do
  let (_getById :<|> retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  retry payoutRequestId

postPayoutPayoutCancel ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  PayoutTypes.PayoutCancelReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCancel merchantShortId opCity payoutRequestId req = do
  let (_getById :<|> _retry :<|> cancelPayout :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  cancelPayout payoutRequestId req

postPayoutPayoutCash ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id PayoutRequest.PayoutRequest ->
  PayoutTypes.PayoutCashUpdateReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCash merchantShortId opCity payoutRequestId req = do
  let (_getById :<|> _retry :<|> _cancel :<|> markCash :<|> _deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  markCash payoutRequestId req

postPayoutPayoutVpaDelete ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  PayoutTypes.DeleteVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaDelete merchantShortId opCity req = do
  let (_getById :<|> _retry :<|> _cancel :<|> _cash :<|> deleteVpa :<|> _updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  deleteVpa req

postPayoutPayoutVpaUpdate ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  PayoutTypes.UpdateVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaUpdate merchantShortId opCity req = do
  let (_getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> updateVpa :<|> _refund) =
        payoutServer merchantShortId opCity
  updateVpa req

postPayoutPayoutVpaRefundRegistration ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  PayoutTypes.RefundRegAmountReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaRefundRegistration merchantShortId opCity req = do
  let (_getById :<|> _retry :<|> _cancel :<|> _cash :<|> _deleteVpa :<|> _updateVpa :<|> refundReg) =
        payoutServer merchantShortId opCity
  refundReg req
