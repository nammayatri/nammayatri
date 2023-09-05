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

module Storage.Beam.KioskLocationTranslation where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data KioskLocationTranslationT f = KioskLocationTranslationT
  { kioskLocationId :: B.C f Text,
    language :: B.C f Language,
    landmark :: B.C f Text,
    address :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table KioskLocationTranslationT where
  data PrimaryKey KioskLocationTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . kioskLocationId

type KioskLocationTranslation = KioskLocationTranslationT Identity

kioskLocationTranslationTMod :: KioskLocationTranslationT (B.FieldModification (B.TableField KioskLocationTranslationT))
kioskLocationTranslationTMod =
  B.tableModification
    { kioskLocationId = B.fieldNamed "kiosk_location_id",
      language = B.fieldNamed "language",
      landmark = B.fieldNamed "landmark",
      address = B.fieldNamed "address"
    }

$(enableKVPG ''KioskLocationTranslationT ['kioskLocationId] [['language]])
$(mkTableInstances ''KioskLocationTranslationT "kiosk_location_translation" "atlas_driver_offer_bpp")
