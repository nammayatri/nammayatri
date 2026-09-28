{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Whether `driver_information.subscribed` is the authority on a driver's eligibility.
--
-- `subscribed` is a postpaid (YATRI_SUBSCRIPTION) dues flag: the DriverFee job clears it
-- when a driver's outstanding balance crosses their plan's credit limit. A prepaid driver
-- has no driver_fee rows, so nothing ever sets it for them, and gating them on it removes
-- them from every pool with no error.
--
-- Two ways to be exempt, and they are deliberately OR'd rather than unified:
--
--   * `ride_billing_model = PREPAID_SUBSCRIPTION` -- the explicit, per-driver answer.
--   * An active fleet association at a merchant running prepaid -- the pre-existing
--     carve-out, kept verbatim. Fleet drivers settle against the fleet owner's wallet and
--     never carry their own `subscribed`, but this only holds where the merchant actually
--     runs prepaid; a fleet on a postpaid merchant is still governed by dues.
--
-- Keeping the carve-out as a separate disjunct (rather than deriving "fleet implies
-- prepaid") makes this strictly additive: everything eligible today stays eligible, and
-- prepaid drivers are added. Nothing that passes now can start failing.
module SharedLogic.Subscription.BillingModel
  ( isPrepaidBillingModel,
    isExemptFromPostpaidDuesFlag,
    isPrepaidDriverWithCatalogueFallback,
  )
where

import Domain.Types.Extra.Plan (ServiceNames (..))
import Kernel.Prelude

-- | The driver's own explicit billing model. Nothing means postpaid -- the behaviour
-- every driver had before the column existed.
isPrepaidBillingModel :: Maybe ServiceNames -> Bool
isPrepaidBillingModel = (== Just PREPAID_SUBSCRIPTION)

-- | Whether to skip the `subscribed` dues check for this driver.
--
-- The first argument is @merchant.prepaidSubscriptionAndWalletEnabled@; the second the
-- driver's active fleet owner, if any -- @ride.fleetOwnerId@ when judging a specific ride,
-- otherwise the live association (or @DriverPoolData.fleetOwnerId@ inside pooling).
isExemptFromPostpaidDuesFlag :: Bool -> Maybe Text -> Maybe ServiceNames -> Bool
isExemptFromPostpaidDuesFlag merchantPrepaidEnabled mbFleetOwnerId mbModel =
  isPrepaidBillingModel mbModel || (merchantPrepaidEnabled && isJust mbFleetOwnerId)

-- | As 'isExemptFromPostpaidDuesFlag', but falls back to the purchasable-services
-- catalogue when the driver has no explicit model yet.
--
-- An explicit model always wins. Delete the Nothing branch once every row is backfilled.
isPrepaidDriverWithCatalogueFallback :: Bool -> Maybe Text -> Maybe ServiceNames -> [ServiceNames] -> Bool
isPrepaidDriverWithCatalogueFallback merchantPrepaidEnabled mbFleetOwnerId mbModel catalogue =
  case mbModel of
    Just _ -> isExemptFromPostpaidDuesFlag merchantPrepaidEnabled mbFleetOwnerId mbModel
    Nothing ->
      PREPAID_SUBSCRIPTION `elem` catalogue
        || (merchantPrepaidEnabled && isJust mbFleetOwnerId)
