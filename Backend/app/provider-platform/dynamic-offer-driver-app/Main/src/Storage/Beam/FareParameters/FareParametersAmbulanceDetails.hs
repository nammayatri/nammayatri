{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FareParameters.FareParametersAmbulanceDetails where

import qualified Database.Beam as B
import Database.Beam.Backend ()
import Kernel.Prelude
import Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersAmbulanceDetailsT f = FareParametersAmbulanceDetailsT
  { fareParametersId :: B.C f Text,
    platformFee :: B.C f (Maybe HighPrecMoney),
    sgst :: B.C f (Maybe HighPrecMoney),
    cgst :: B.C f (Maybe HighPrecMoney),
    currency :: B.C f Currency,
    distBasedFare :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersAmbulanceDetailsT where
  data PrimaryKey FareParametersAmbulanceDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

type FareParametersAmbulanceDetails = FareParametersAmbulanceDetailsT Identity

$(enableKVPG ''FareParametersAmbulanceDetailsT ['fareParametersId] [])

$(mkTableInstances ''FareParametersAmbulanceDetailsT "fare_parameters_ambulance_details")
