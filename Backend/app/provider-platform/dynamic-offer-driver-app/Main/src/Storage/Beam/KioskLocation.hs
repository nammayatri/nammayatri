{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.KioskLocation where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Sequelize
import Tools.Beam.UtilsTH (enableKVPG, mkTableInstances)

data KioskLocationT f = KioskLocationT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    address :: B.C f Text,
    landmark :: B.C f Text,
    contact :: B.C f Text,
    longitude :: B.C f Double,
    latitude :: B.C f Double
  }
  deriving (Generic, B.Beamable)

instance B.Table KioskLocationT where
  data PrimaryKey KioskLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type KioskLocation = KioskLocationT Identity

$(enableKVPG ''KioskLocationT ['id] [['merchantId]])

$(mkTableInstances ''KioskLocationT "kiosk_location")
