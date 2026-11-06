{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyInterCityDetails where

import Data.Aeson as DA
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Domain.Types.Common
import Domain.Types.Extra.IndianState ()
import Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as Reexport
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Domain
import qualified Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection as DFFPICDPKRS
import Kernel.Prelude
import Kernel.Types.Beckn.Context (IndianState)
import Kernel.Types.Common

data FPInterCityDetailsD (s :: UsageSafety) = FPInterCityDetails
  { baseFare :: HighPrecMoney,
    perHourCharge :: HighPrecMoney,
    perKmRateOneWay :: HighPrecMoney,
    perKmRateRoundTrip :: HighPrecMoney,
    perKmRateSectionsOneWay :: Maybe (NonEmpty FPInterCityDetailsPerKmRateSection), -- when set, one-way trips are priced by these instead of perKmRateOneWay
    perKmRateSectionsRoundTrip :: Maybe (NonEmpty FPInterCityDetailsPerKmRateSection), -- when set, round trips are priced by these instead of perKmRateRoundTrip
    perExtraKmRate :: HighPrecMoney,
    perExtraMinRate :: HighPrecMoney,
    kmPerPlannedExtraHour :: Kilometers,
    deadKmFare :: HighPrecMoney,
    perDayMaxHourAllowance :: Hours,
    perDayMaxAllowanceInMins :: Maybe Minutes,
    pricingSlabs :: NonEmpty (FPInterCityDetailsPricingSlabsD s),
    defaultWaitTimeAtDestination :: Minutes,
    currency :: Currency,
    stateEntryPermitCharges :: Maybe HighPrecMoney, -- this trip's charge: not read from the table, search sets it from stateEntryPermitChargesByState
    stateEntryPermitChargesByState :: Maybe (Map IndianState HighPrecMoney),
    nightShiftCharge :: Maybe Domain.NightShiftCharge,
    waitingChargeInfo :: Maybe Domain.WaitingChargeInfo
  }
  deriving (Generic, Show, ToSchema)

type FPInterCityDetails = FPInterCityDetailsD 'Safe

instance FromJSON (FPInterCityDetailsD 'Unsafe)

instance ToJSON (FPInterCityDetailsD 'Unsafe)

-- FIXME remove
instance FromJSON (FPInterCityDetailsD 'Safe)

-- FIXME remove
instance ToJSON (FPInterCityDetailsD 'Safe)

-- | Priced like progressive's per extra km rate sections: each planned km is charged at the rate of the section it
-- falls in, a section running from its start distance to the next one's, so the first section starts at 0.
data FPInterCityDetailsPerKmRateSection = FPInterCityDetailsPerKmRateSection
  { startDistance :: Meters,
    perKmRate :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | A fare policy's section rows, split into its one-way and round-trip sections, each ordered by start distance.
mkPerKmRateSections :: [DFFPICDPKRS.FullFarePolicyInterCityDetailsPerKmRateSection] -> (Maybe (NonEmpty FPInterCityDetailsPerKmRateSection), Maybe (NonEmpty FPInterCityDetailsPerKmRateSection))
mkPerKmRateSections rows = (sectionsFor False, sectionsFor True)
  where
    sectionsFor isRoundTrip =
      NE.sortWith (.startDistance)
        <$> NE.nonEmpty [FPInterCityDetailsPerKmRateSection {startDistance = row.startDistance, perKmRate = row.perKmRate} | row <- rows, row.isRoundTrip == isRoundTrip]
