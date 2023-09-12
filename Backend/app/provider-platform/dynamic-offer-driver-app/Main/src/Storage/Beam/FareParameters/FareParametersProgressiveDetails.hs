{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FareParameters.FareParametersProgressiveDetails where

import qualified Database.Beam as B
import Database.Beam.Backend ()
import Kernel.Prelude
import Kernel.Types.Common (Money)
import Tools.Beam.UtilsTH

data FareParametersProgressiveDetailsT f = FareParametersProgressiveDetailsT
  { fareParametersId :: B.C f Text,
    deadKmFare :: B.C f Money,
    extraKmFare :: B.C f (Maybe Money)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersProgressiveDetailsT where
  data PrimaryKey FareParametersProgressiveDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

type FareParametersProgressiveDetails = FareParametersProgressiveDetailsT Identity

$(enableKVPG ''FareParametersProgressiveDetailsT ['fareParametersId] [])

$(mkTableInstances ''FareParametersProgressiveDetailsT "fare_parameters_progressive_details")
