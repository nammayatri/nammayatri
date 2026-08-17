{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

-- | Split out of Domain.Types.FarePolicy so Domain.Types.ConditionalCharges can import
-- PlatformFeeMethods without a cycle: FarePolicy already imports ConditionalCharges (for its
-- conditionalCharges field), so ConditionalCharges importing FarePolicy back would be circular.
module Domain.Types.FarePolicy.PlatformFeeMethods where

import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

-- | PlanBased behaves exactly like FixedAmount, except the charge is dropped when the driver's
-- plan sets waivesSpecialRideCharges (Daily Unlimited). Marking a fare policy PlanBased is what
-- makes its charge waivable at all -- a plan can never waive a FixedAmount charge, so Airport /
-- Intercity / Rental stay billed regardless of plan by simply staying FixedAmount.
data PlatformFeeMethods = Subscription | FixedAmount | None | SlabBased | NoCharge | PlanBased
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlatformFeeMethods

$(mkBeamInstancesForEnum ''PlatformFeeMethods)
