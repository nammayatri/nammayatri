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
import Prelude (Show (..))

data CacPrefix
  = DriverPoolConfig
  | FarePolicy
  | TransporterConfig
  | DriverIntelligentPoolConfig
  | GoHomeConfig
  | FarePolicyDriverExtraFeeBounds
  | FarePolicyProgressiveDetailsPerExtraKmRateSection
  | FarePolicyProgressiveDetails
  | FarePolicySlabsDetailsSlab
  | FarePolicyRentalDetails
  | FarePolicyRentalDetailsDistanceBuffers
  | MerchantServiceUsageConfig
  | Empty
  deriving (Eq)

instance Show CacPrefix where
  show DriverPoolConfig = "driverPoolConfig:"
  show FarePolicy = "farePolicy:"
  show TransporterConfig = "transporterConfig:"
  show DriverIntelligentPoolConfig = "driverIntelligentPoolConfig:"
  show GoHomeConfig = "goHomeConfig:"
  show FarePolicyDriverExtraFeeBounds = "farePolicyDriverExtraFeeBounds:"
  show FarePolicyProgressiveDetailsPerExtraKmRateSection = "farePolicyProgressiveDetailsPerExtraKmRateSection:"
  show FarePolicyProgressiveDetails = "farePolicyProgressiveDetails:"
  show FarePolicySlabsDetailsSlab = "farePolicySlabsDetailsSlab:"
  show FarePolicyRentalDetails = "farePolicyRentalDetails:"
  show FarePolicyRentalDetailsDistanceBuffers = "farePolicyRentalDetailsDistanceBuffers:"
  show MerchantServiceUsageConfig = "merchantServiceUsageConfig:"
  show Empty = ""

getTableName :: CacPrefix -> Text
getTableName DriverPoolConfig = "driver_pool_config"
getTableName FarePolicy = "fare_policy"
getTableName TransporterConfig = "transporter_config"
getTableName DriverIntelligentPoolConfig = "driver_intelligent_pool_config"
getTableName GoHomeConfig = "go_home_config"
getTableName FarePolicyDriverExtraFeeBounds = "fare_policy_driver_extra_fee_bounds"
getTableName FarePolicyProgressiveDetailsPerExtraKmRateSection = "fare_policy_progressive_details_per_extra_km_rate_section"
getTableName FarePolicyProgressiveDetails = "fare_policy_progressive_details"
getTableName FarePolicySlabsDetailsSlab = "fare_policy_slabs_details_slab"
getTableName FarePolicyRentalDetails = "fare_policy_rental_details"
getTableName FarePolicyRentalDetailsDistanceBuffers = "fare_policy_rental_details_distance_buffers"
getTableName MerchantServiceUsageConfig = "merchant_service_usage_config"
getTableName Empty = ""
