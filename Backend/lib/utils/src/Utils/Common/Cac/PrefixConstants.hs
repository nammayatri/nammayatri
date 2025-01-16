{-
	Copyright 2022-23, Juspay India Pvt Ltd

	This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

	as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

	is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

	or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.Cac.PrefixConstants where

import Kernel.Prelude as KP
import Utils.Common.Cac.UtilsTH
import Prelude (Show (..))

data CacPrefix
  = DriverPoolConfig
  | FarePolicy
  | TransporterConfig
  | DriverIntelligentPoolConfig
  | GoHomeConfig
  | FarePolicyDriverExtraFeeBounds
  | FarePolicyProgressiveDetailsPerExtraKmRateSection
  | FullFarePolicyProgressiveDetailsPerMinRateSection
  | FarePolicyProgressiveDetails
  | FarePolicySlabsDetailsSlab
  | FarePolicyRentalDetails
  | FarePolicyInterCityDetails
  | FarePolicyAmbulanceDetailsSlab
  | FarePolicyRentalDetailsDistanceBuffers
  | FarePolicyRentalDetailsPricingSlabs
  | FarePolicyInterCityDetailsPricingSlabs
  | MerchantServiceUsageConfig
  | Empty
  deriving (Eq, Bounded)

instance Show CacPrefix where
  show DriverPoolConfig = "driverPoolConfig:"
  show FarePolicy = "farePolicy:"
  show TransporterConfig = "transporterConfig:"
  show DriverIntelligentPoolConfig = "driverIntelligentPoolConfig:"
  show GoHomeConfig = "goHomeConfig:"
  show FarePolicyDriverExtraFeeBounds = "farePolicyDriverExtraFeeBounds:"
  show FarePolicyProgressiveDetailsPerExtraKmRateSection = "farePolicyProgressiveDetailsPerExtraKmRateSection:"
  show FullFarePolicyProgressiveDetailsPerMinRateSection = "fullFarePolicyProgressiveDetailsPerMinRateSection:"
  show FarePolicyProgressiveDetails = "farePolicyProgressiveDetails:"
  show FarePolicySlabsDetailsSlab = "farePolicySlabsDetailsSlab:"
  show FarePolicyRentalDetails = "farePolicyRentalDetails:"
  show FarePolicyInterCityDetails = "farePolicyInterCityDetails:"
  show FarePolicyAmbulanceDetailsSlab = "farePolicyAmbulanceDetailsSlab:"
  show FarePolicyRentalDetailsDistanceBuffers = "farePolicyRentalDetailsDistanceBuffers:"
  show FarePolicyRentalDetailsPricingSlabs = "farePolicyRentalDetailsPricingSlabs:"
  show FarePolicyInterCityDetailsPricingSlabs = "farePolicyInterCityDetailsPricingSlabs:"
  show MerchantServiceUsageConfig = "merchantServiceUsageConfig:"
  show Empty = ""

$(mkCacFunction ''CacPrefix "getCacMetricErrorFromCac" "_from_cac_parse_error")
$(mkCacFunction ''CacPrefix "getCacMetricErrorFromDB" "_from_db_parse_error")
$(mkCacFunction ''CacPrefix "getTableName" "")
