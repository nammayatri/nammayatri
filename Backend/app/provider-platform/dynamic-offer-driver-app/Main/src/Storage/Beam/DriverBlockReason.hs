{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverBlockReason where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverBlockReasonT f = DriverBlockReasonT
  { reasonCode :: B.C f Text,
    blockReason :: B.C f (Maybe Text),
    blockTimeInHours :: B.C f (Maybe Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverBlockReasonT where
  data PrimaryKey DriverBlockReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . reasonCode

type DriverBlockReason = DriverBlockReasonT Identity

$(enableKVPG ''DriverBlockReasonT ['reasonCode] [])

$(mkTableInstances ''DriverBlockReasonT "driver_block_reason")
