{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Who pays for a driver's rides, and how.
--
-- Two inputs decide it:
--
--   * The driver's own `ride_billing_model` -- YATRI_SUBSCRIPTION (postpaid dues) or
--     PREPAID_SUBSCRIPTION (prepaid credits). Every driver has one: registration writes it
--     and feature migration 0070 backfilled the rest.
--   * An active fleet association. At a merchant running prepaid the fleet owner pays from
--     their own prepaid balance, whatever the driver's own model says; the driver's model
--     only takes effect again if they leave the fleet.
--
-- Every gate that asks "is this driver prepaid?" goes through 'resolvePayer', so pooling,
-- dispatch, go-online, OTP rides and ride start cannot disagree about the same driver.
module SharedLogic.Subscription.BillingModel
  ( Payer (..),
    resolvePayer,
    isPrepaidPayer,
    modelOrCatalogue,
  )
where

import Control.Applicative ((<|>))
import Domain.Types.Extra.Plan (ServiceNames (..))
import Domain.Types.SubscriptionPurchase (SubscriptionOwnerType (..))
import Kernel.Prelude

data Payer = Payer
  { -- | Whose prepaid balance and purchases a prepaid payer is checked against. Postpaid
    -- dues are always the driver's own, so these only matter when 'model' is PREPAID.
    ownerType :: SubscriptionOwnerType,
    ownerId :: Text,
    model :: ServiceNames
  }
  deriving (Show, Eq)

-- | Resolve the payer for a driver.
--
-- Arguments: @merchant.prepaidSubscriptionAndWalletEnabled@; the driver's id; the active
-- fleet owner, if any (@ride.fleetOwnerId@ when judging a specific ride, otherwise the live
-- association, or @DriverPoolData.fleetOwnerId@ inside pooling); the driver's own model,
-- where null reads as YATRI_SUBSCRIPTION (see 'modelOrCatalogue' for callers that have the
-- catalogue).
--
-- A fleet driver's owner is the fleet owner even at a merchant without prepaid, matching
-- end-ride and go-online, which have always charged prepaid against the fleet owner. Only
-- the model differs: there the driver's own model still applies.
resolvePayer :: Bool -> Text -> Maybe Text -> Maybe ServiceNames -> Payer
resolvePayer merchantPrepaidEnabled driverId mbFleetOwnerId mbDriverModel =
  let driverModel = fromMaybe YATRI_SUBSCRIPTION mbDriverModel
   in case mbFleetOwnerId of
        Just fleetOwnerId ->
          Payer
            { ownerType = FLEET_OWNER,
              ownerId = fleetOwnerId,
              model = if merchantPrepaidEnabled then PREPAID_SUBSCRIPTION else driverModel
            }
        Nothing ->
          Payer
            { ownerType = DRIVER,
              ownerId = driverId,
              model = driverModel
            }

-- | Whether `driver_information.subscribed` should be skipped for this payer.
--
-- `subscribed` is a postpaid dues flag: the DriverFee job clears it when a driver's
-- outstanding balance crosses their plan's credit limit. A prepaid payer has no driver_fee
-- rows, so nothing ever sets it for them, and gating them on it removes them from every
-- pool with no error.
isPrepaidPayer :: Payer -> Bool
isPrepaidPayer payer = payer.model == PREPAID_SUBSCRIPTION

-- | The driver's own model, for callers that also have the catalogue
-- (`servicesEnabledForSubscription`).
--
-- A null model only remains on a row that registered on the old code after 0070 ran; it
-- resolves the way go-online always resolved it: PREPAID if the catalogue offers it.
-- Pooling has no catalogue and reads null as YATRI, which is also what it did before.
-- Delete this once 0070 has been re-run after the deploy.
modelOrCatalogue :: Maybe ServiceNames -> [ServiceNames] -> Maybe ServiceNames
modelOrCatalogue mbModel catalogue =
  mbModel <|> (PREPAID_SUBSCRIPTION <$ guard (PREPAID_SUBSCRIPTION `elem` catalogue))
