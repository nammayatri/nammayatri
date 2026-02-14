{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.ReferralPayout
  ( postReferralPayoutDashboardPayoutDeleteVpa,
    getReferralPayoutDashboardPayoutRegistration,
    getReferralPayoutDashboardPayoutOrderStatus,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.ReferralPayout as Common
import qualified "dashboard-helper-api" Dashboard.Common as DCommon
import Control.Monad (join)
import qualified Domain.Action.UI.ReferralPayout as DReferralPayout
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import qualified Kernel.External.Payout.Interface.Types as Payout
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

-- Dashboard calls these; they delegate to the same UI handlers used by driver app (payout/delete/vpa, payout/registration, payout/order/status).

postReferralPayoutDashboardPayoutDeleteVpa ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe (Maybe (Id DCommon.Driver)) ->
  Flow APISuccess
postReferralPayoutDashboardPayoutDeleteVpa merchantShortId opCity mbDriverId = do
  driverId <- fromMaybeM (InvalidRequest "driverId required for referral payout") (join mbDriverId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @DCommon.Driver @DP.Person driverId
  DReferralPayout.postPayoutDeleteVpa (Just personId, merchant.id, merchantOpCityId)

getReferralPayoutDashboardPayoutRegistration ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe (Maybe (Id DCommon.Driver)) ->
  Flow Common.ReferralPayoutRegistrationRes
getReferralPayoutDashboardPayoutRegistration merchantShortId opCity mbDriverId = do
  driverId <- fromMaybeM (InvalidRequest "driverId required for referral payout") (join mbDriverId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @DCommon.Driver @DP.Person driverId
  clearDuesRes <- DReferralPayout.getPayoutRegistration (Just personId, merchant.id, merchantOpCityId)
  pure $
    Common.ReferralPayoutRegistrationRes
      { orderId = clearDuesRes.orderId,
        orderResp = clearDuesRes.orderResp
      }

getReferralPayoutDashboardPayoutOrderStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe (Maybe (Id DCommon.Driver)) ->
  Text ->
  Flow Payout.PayoutOrderStatusResp
getReferralPayoutDashboardPayoutOrderStatus merchantShortId opCity mbDriverId orderId = do
  driverId <- fromMaybeM (InvalidRequest "driverId required for referral payout") (join mbDriverId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @DCommon.Driver @DP.Person driverId
  DReferralPayout.getPayoutOrderStatus (Just personId, merchant.id, merchantOpCityId) orderId
