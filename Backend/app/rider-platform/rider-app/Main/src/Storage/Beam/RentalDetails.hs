{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.RentalDetails where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

-- TODO To be removed
data RentalDetailsT f = RentalDetailsT
  { id :: B.C f Text,
    baseFare :: B.C f Money,
    perHourCharge :: B.C f Money,
    perHourFreeKms :: B.C f Int,
    perExtraKmRate :: B.C f Money,
    nightShiftCharge :: B.C f (Maybe Money),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay)
  }
  deriving (Generic, B.Beamable)

instance B.Table RentalDetailsT where
  data PrimaryKey RentalDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RentalDetails = RentalDetailsT Identity

$(enableKVPG ''RentalDetailsT ['id] [])

$(mkTableInstances ''RentalDetailsT "rental_details")
