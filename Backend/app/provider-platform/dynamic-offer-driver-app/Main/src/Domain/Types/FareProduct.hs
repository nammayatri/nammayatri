{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FareProduct where

import Data.Time
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as FarePolicyD
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Lib.Types.SpecialLocation (Area (..))
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data BoundedPeaks = BoundedPeaks
  { monday :: [(TimeOfDay, TimeOfDay)],
    tuesday :: [(TimeOfDay, TimeOfDay)],
    wednesday :: [(TimeOfDay, TimeOfDay)],
    thursday :: [(TimeOfDay, TimeOfDay)],
    friday :: [(TimeOfDay, TimeOfDay)],
    saturday :: [(TimeOfDay, TimeOfDay)],
    sunday :: [(TimeOfDay, TimeOfDay)]
  }
  deriving (Eq, Ord, Generic, Show, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable BoundedPeaks

data TimeBound
  = BoundedByWeekday BoundedPeaks
  | BoundedByDay [(Day, [(TimeOfDay, TimeOfDay)])]
  | Unbounded
  deriving (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TimeBound

instance Show TimeBound where
  show Unbounded = "Unbounded"
  show (BoundedByWeekday peaks) = show peaks
  show (BoundedByDay days) = show days

instance Read TimeBound where
  readsPrec _ str
    | str == "Unbounded" = [(Unbounded, "")]
    | otherwise =
      case (readMaybe str :: Maybe BoundedPeaks) of
        Just bound -> [(BoundedByWeekday bound, "")]
        Nothing ->
          case (readMaybe str :: Maybe [(Day, [(TimeOfDay, TimeOfDay)])]) of
            Just bound -> [(BoundedByDay bound, "")]
            Nothing -> [(Unbounded, "")]

$(mkBeamInstancesForEnum ''TimeBound)

data FareProduct = FareProduct
  { id :: Id FareProduct,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    farePolicyId :: Id FarePolicyD.FarePolicy,
    vehicleServiceTier :: DVST.ServiceTierType,
    area :: Area,
    tripCategory :: DTC.TripCategory,
    timeBounds :: TimeBound,
    enabled :: Bool
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON, ToJSON)
