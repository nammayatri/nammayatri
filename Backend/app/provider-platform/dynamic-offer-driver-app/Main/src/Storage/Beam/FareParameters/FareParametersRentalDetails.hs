{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FareParameters.FareParametersRentalDetails where

import qualified Database.Beam as B
import Database.Beam.Backend ()
import Kernel.Prelude
import Kernel.Types.Common (Money)
import Tools.Beam.UtilsTH

data FareParametersRentalDetailsT f = FareParametersRentalDetailsT
  { fareParametersId :: B.C f Text,
    timeBasedFare :: B.C f Money,
    extraDistFare :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersRentalDetailsT where
  data PrimaryKey FareParametersRentalDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

type FareParametersRentalDetails = FareParametersRentalDetailsT Identity

$(enableKVPG ''FareParametersRentalDetailsT ['fareParametersId] [])

$(mkTableInstances ''FareParametersRentalDetailsT "fare_parameters_rental_details")
