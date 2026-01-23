{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FareParameters.FareParametersInterCityDetails where

import qualified Database.Beam as B
import Database.Beam.Backend ()
import Kernel.Prelude
import Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersInterCityDetailsT f = FareParametersInterCityDetailsT
  { fareParametersId :: B.C f Text,
    timeFare :: B.C f HighPrecMoney,
    distanceFare :: B.C f HighPrecMoney,
    pickupCharge :: B.C f HighPrecMoney,
    currency :: B.C f Currency,
    extraDistanceFare :: B.C f HighPrecMoney,
    extraTimeFare :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersInterCityDetailsT where
  data PrimaryKey FareParametersInterCityDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

type FareParametersInterCityDetails = FareParametersInterCityDetailsT Identity

$(enableKVPG ''FareParametersInterCityDetailsT ['fareParametersId] [])

$(mkTableInstances ''FareParametersInterCityDetailsT "fare_parameters_inter_city_details")
