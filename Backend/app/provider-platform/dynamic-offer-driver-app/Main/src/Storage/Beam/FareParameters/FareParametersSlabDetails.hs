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

module Storage.Beam.FareParameters.FareParametersSlabDetails where

import qualified Database.Beam as B
import Database.Beam.Backend ()
import qualified Domain.Types.FareParameters as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KId
import Tools.Beam.UtilsTH

data FareParametersSlabDetailsT f = FareParametersSlabDetailsT
  { fareParametersId :: B.C f Text,
    platformFee :: B.C f (Maybe HighPrecMoney),
    sgst :: B.C f (Maybe HighPrecMoney),
    cgst :: B.C f (Maybe HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersSlabDetailsT where
  data PrimaryKey FareParametersSlabDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

type FareParametersSlabDetails = FareParametersSlabDetailsT Identity

type FullFareParametersSlabDetails = (KId.Id Domain.FareParameters, Domain.FParamsSlabDetails)

$(enableKVPG ''FareParametersSlabDetailsT ['fareParametersId] [])

$(mkTableInstances ''FareParametersSlabDetailsT "fare_parameters_slab_details")
