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

module Storage.Beam.Exophone where

import qualified Database.Beam as B
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import Tools.Beam.UtilsTH

data ExophoneT f = ExophoneT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    primaryPhone :: B.C f Text,
    backupPhone :: B.C f Text,
    isPrimaryDown :: B.C f Bool,
    callService :: B.C f CallService,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ExophoneT where
  data PrimaryKey ExophoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Exophone = ExophoneT Identity

$(enableKVPG ''ExophoneT ['id] [['merchantOperatingCityId], ['primaryPhone], ['backupPhone]])

$(mkTableInstances ''ExophoneT "exophone")
