module Domain.Action.Dashboard.AppManagement.DriverWallet
  ( getDriverWalletWalletTransactions,
    postDriverWalletWalletPayout,
    postDriverWalletWalletTopup,
    getDriverWalletWalletPayoutHistory,
  )
where

import qualified API.Types.UI.DriverWallet as DriverWallet
import qualified Domain.Action.UI.DriverWallet as DDriverWallet
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getDriverWalletWalletTransactions ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DP.Person ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow DriverWallet.WalletSummaryResponse
getDriverWalletWalletTransactions merchantShortId opCity driverId mbFromDate mbToDate = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Kernel.Prelude.Nothing merchant (Kernel.Prelude.Just opCity)
  DDriverWallet.getWalletTransactions (Kernel.Prelude.Just driverId, merchant.id, merchantOpCityId) mbFromDate mbToDate

postDriverWalletWalletPayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DP.Person ->
  Environment.Flow APISuccess.APISuccess
postDriverWalletWalletPayout merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Kernel.Prelude.Nothing merchant (Kernel.Prelude.Just opCity)
  DDriverWallet.postWalletPayout (Kernel.Prelude.Just driverId, merchant.id, merchantOpCityId)

postDriverWalletWalletTopup ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DP.Person ->
  DriverWallet.TopUpRequest ->
  Environment.Flow Plan.PlanSubscribeRes
postDriverWalletWalletTopup merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Kernel.Prelude.Nothing merchant (Kernel.Prelude.Just opCity)
  DDriverWallet.postWalletTopup (Kernel.Prelude.Just driverId, merchant.id, merchantOpCityId) req

getDriverWalletWalletPayoutHistory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DP.Person ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe [PR.PayoutRequestStatus] ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow DriverWallet.PayoutHistoryResponse
getDriverWalletWalletPayoutHistory merchantShortId opCity driverId mbFrom mbTo mbStatuses mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Kernel.Prelude.Nothing merchant (Kernel.Prelude.Just opCity)
  DDriverWallet.getWalletPayoutHistory (Kernel.Prelude.Just driverId, merchant.id, merchantOpCityId) mbFrom mbTo mbStatuses mbLimit mbOffset
