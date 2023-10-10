{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.QuoteRental where

import qualified Database.Beam as B
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data QuoteRentalT f = QuoteRentalT
  { id :: B.C f Text,
    searchRequestId :: B.C f Text,
    providerId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    validTill :: B.C f LocalTime,
    fareParametersId :: B.C f Text,
    estimatedFinishTime :: B.C f UTCTime,
    baseDistance :: B.C f Kilometers,
    baseDuration :: B.C f Hours,
    baseFare :: B.C f Money,
    createdAt :: B.C f LocalTime,
    updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteRentalT where
  data PrimaryKey QuoteRentalT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type QuoteRental = QuoteRentalT Identity

$(enableKVPG ''QuoteRentalT ['id] [['searchRequestId]])
$(mkTableInstances ''QuoteRentalT "quote_rental")
