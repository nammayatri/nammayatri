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

module Storage.Beam.CancellationReason where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data CancellationReasonT f = CancellationReasonT
  { reasonCode :: B.C f Text,
    description :: B.C f Text,
    enabled :: B.C f Bool,
    priority :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table CancellationReasonT where
  data PrimaryKey CancellationReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . reasonCode

type CancellationReason = CancellationReasonT Identity

$(enableKVPG ''CancellationReasonT ['reasonCode] [])

$(mkTableInstances ''CancellationReasonT "cancellation_reason")
