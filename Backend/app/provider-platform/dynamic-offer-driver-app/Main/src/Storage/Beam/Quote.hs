{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.Quote where

import qualified Database.Beam as B
import Domain.Types
import Domain.Types.Common ()
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Tools.Beam.UtilsTH

data QuoteSpecialZoneT f = QuoteSpecialZoneT
  { id :: B.C f Text,
    searchRequestId :: B.C f Text,
    providerId :: B.C f Text,
    vehicleVariant :: B.C f ServiceTierType,
    estimatedFinishTime :: B.C f (Maybe UTCTime),
    vehicleServiceTierName :: B.C f (Maybe Text),
    tripCategory :: B.C f (Maybe TripCategory),
    distance :: B.C f (Maybe Common.Meters),
    distanceUnit :: B.C f (Maybe Common.DistanceUnit),
    validTill :: B.C f LocalTime,
    estimatedFare :: B.C f Common.Money,
    estimatedFareAmount :: B.C f (Maybe Common.HighPrecMoney),
    currency :: B.C f (Maybe Common.Currency),
    specialLocationTag :: B.C f (Maybe Text),
    fareParametersId :: B.C f Text,
    farePolicyId :: B.C f (Maybe Text),
    isScheduled :: B.C f (Maybe Bool),
    isCustomerPrefferedSearchRoute :: B.C f (Maybe Bool),
    isBlockedRoute :: B.C f (Maybe Bool),
    tollNames :: B.C f (Maybe [Text]),
    createdAt :: B.C f LocalTime,
    updatedAt :: B.C f LocalTime,
    merchantOperatingCityId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteSpecialZoneT where
  data PrimaryKey QuoteSpecialZoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type QuoteSpecialZone = QuoteSpecialZoneT Identity

$(enableKVPG ''QuoteSpecialZoneT ['id] [['searchRequestId]])

$(mkTableInstances ''QuoteSpecialZoneT "quote_special_zone")
