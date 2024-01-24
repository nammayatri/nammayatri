{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Quote where

import qualified Database.Beam as B
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Tools.Beam.UtilsTH

data QuoteT f = QuoteT
  { id :: B.C f Text,
    searchRequestId :: B.C f Text,
    providerId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    estimatedFinishTime :: B.C f (Maybe UTCTime),
    tripCategory :: B.C f (Maybe DTC.TripCategory),
    distance :: B.C f (Maybe Common.Meters),
    validTill :: B.C f UTCTime,
    estimatedFare :: B.C f Common.Money,
    specialLocationTag :: B.C f (Maybe Text),
    fareParametersId :: B.C f Text,
    farePolicyId :: B.C f (Maybe Text),
    isScheduled :: B.C f (Maybe Bool),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteT where
  data PrimaryKey QuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Quote = QuoteT Identity

$(enableKVPG ''QuoteT ['id] [['searchRequestId]])

{-
  The table named quote_special_zone in the QuoteT database was originally created exclusively for handling
  data pertaining to special zones. This naming convention has been retained for reasons of backward compatibility.
  Over time, the use of this table was expanded beyond its initial scope to accommodate a broader range of use cases,
  extending its functionality beyond just special zones
-}
$(mkTableInstances ''QuoteT "quote_special_zone")
