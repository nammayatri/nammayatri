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
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Beam.DriverOnboarding.Image where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.DriverOnboarding.Error as Domain
import qualified Domain.Types.DriverOnboarding.Image as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data ImageT f = ImageT
  { id :: B.C f Text,
    personId :: B.C f Text,
    merchantId :: B.C f Text,
    s3Path :: B.C f Text,
    imageType :: B.C f Domain.ImageType,
    isValid :: B.C f Bool,
    failureReason :: B.C f (Maybe Domain.DriverOnboardingError),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ImageT where
  data PrimaryKey ImageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Image = ImageT Identity

imageTMod :: ImageT (B.FieldModification (B.TableField ImageT))
imageTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      personId = B.fieldNamed "person_id",
      merchantId = B.fieldNamed "merchant_id",
      s3Path = B.fieldNamed "s3_path",
      imageType = B.fieldNamed "image_type",
      isValid = B.fieldNamed "is_valid",
      failureReason = B.fieldNamed "failure_reason",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''ImageT ['id] [['personId], ['merchantId]]) -- DON'T Enable for KV

$(mkTableInstances ''ImageT "image" "atlas_driver_offer_bpp")
