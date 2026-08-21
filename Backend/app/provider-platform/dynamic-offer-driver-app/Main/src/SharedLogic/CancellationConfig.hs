{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Readers for @TransporterConfig.cancellationConfig@.
--
-- Every switch governing cancellation behaviour is resolved here, so a merchant's
-- cancellation profile is one JSON object to copy and every call site agrees on what an
-- unset field means.
module SharedLogic.CancellationConfig
  ( carryForwardEnabled,
    preferOndcCancellationReasonId,
    consumeRideCreditOnCancellation,
    cancellationGracePeriodSeconds,
    noShowAcceptableWaitPeriodSeconds,
  )
where

import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude

-- | Whether a cancellation charge is added to the rider's running balance for collection on
-- a later fare.
--
-- Defaults to @canAddCancellationFee@, which is exactly when carry-forward happens today, so
-- an unset config preserves current behaviour for every merchant. Set @False@ for a seller
-- app whose Buyer App collects at cancellation: a balance accrued there could never be
-- cleared, and would charge the rider a second time on their next quote.
carryForwardEnabled :: DTC.TransporterConfig -> Bool
carryForwardEnabled transporterConfig =
  fromMaybe transporterConfig.canAddCancellationFee $
    transporterConfig.cancellationConfig >>= (.carryForwardEnabled)

-- | Whether to resolve the internal reason code from ONDC's @cancellation_reason_id@ enum
-- rather than the buyer app's free-text @short_desc@. Off by default, and off when the
-- config could not be loaded.
preferOndcCancellationReasonId :: Maybe DTC.TransporterConfig -> Bool
preferOndcCancellationReasonId mbTransporterConfig =
  fromMaybe False $
    mbTransporterConfig >>= (.cancellationConfig) >>= (.preferOndcCancellationReasonId)

consumeRideCreditOnCancellation :: DTC.TransporterConfig -> Bool
consumeRideCreditOnCancellation transporterConfig =
  fromMaybe False $
    transporterConfig.cancellationConfig >>= (.consumeRideCreditOnCancellation)

-- | Grace window after booking in which a pre-arrival customer cancellation is free.
-- Zero when unset, so a merchant that has not configured one keeps charging from the
-- moment of booking, as before.
cancellationGracePeriodSeconds :: DTC.TransporterConfig -> Int
cancellationGracePeriodSeconds transporterConfig =
  fromMaybe 0 $
    transporterConfig.cancellationConfig >>= (.cancellationGracePeriodSeconds)

-- | Minimum driver wait at pickup before a customer no-show may be charged.
-- Zero when unset, leaving the decision entirely to the fee rules.
noShowAcceptableWaitPeriodSeconds :: DTC.TransporterConfig -> Int
noShowAcceptableWaitPeriodSeconds transporterConfig =
  fromMaybe 0 $
    transporterConfig.cancellationConfig >>= (.noShowAcceptableWaitPeriodSeconds)
