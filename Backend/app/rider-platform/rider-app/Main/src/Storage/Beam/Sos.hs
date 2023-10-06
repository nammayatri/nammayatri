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

module Storage.Beam.Sos where

import qualified Database.Beam as B
import qualified Domain.Types.Sos as Domain
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data SosT f = SosT
  { id :: B.C f Text,
    personId :: B.C f Text,
    rideId :: B.C f Text,
    flow :: B.C f Domain.SosType,
    status :: B.C f Domain.SosStatus,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SosT where
  data PrimaryKey SosT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Sos = SosT Identity

$(TH.enableKVPG ''SosT ['id] [])

$(TH.mkTableInstances ''SosT "sos")
