{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Time (Seconds)
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsDistanceBuffersT f = FarePolicyRentalDetailsDistanceBuffersT
  { farePolicyId :: B.C f Text,
    rideDuration :: B.C f Seconds,
    bufferKms :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsDistanceBuffersT where
  data PrimaryKey FarePolicyRentalDetailsDistanceBuffersT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyRentalDetailsDistanceBuffers = FarePolicyRentalDetailsDistanceBuffersT Identity

type FullFarePolicyRentalDetailsDistanceBuffers = (KTI.Id Domain.FarePolicy, Domain.FPRentalDetailsDistanceBuffers)

$(enableKVPG ''FarePolicyRentalDetailsDistanceBuffersT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyRentalDetailsDistanceBuffersT "fare_policy_rental_details_distance_buffers")
