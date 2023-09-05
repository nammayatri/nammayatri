{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Message.Message where

import Data.Serialize hiding (label)
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Message.Message as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Lib.Utils ()
import Sequelize

data MessageT f = MessageT
  { id :: B.C f Text,
    messageType :: B.C f Domain.MessageType,
    title :: B.C f Text,
    description :: B.C f Text,
    shortDescription :: B.C f Text,
    label :: B.C f (Maybe Text),
    likeCount :: B.C f Int,
    viewCount :: B.C f Int,
    mediaFiles :: B.C f [Text],
    merchantId :: B.C f Text,
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageT where
  data PrimaryKey MessageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Message = MessageT Identity

messageTMod :: MessageT (B.FieldModification (B.TableField MessageT))
messageTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      messageType = B.fieldNamed "type",
      title = B.fieldNamed "title",
      description = B.fieldNamed "description",
      shortDescription = B.fieldNamed "short_description",
      label = B.fieldNamed "label",
      likeCount = B.fieldNamed "like_count",
      viewCount = B.fieldNamed "view_count",
      mediaFiles = B.fieldNamed "media_files",
      merchantId = B.fieldNamed "merchant_id",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MessageT ['id] []) -- DON'T Enable for KV

$(mkTableInstances ''MessageT "message" "atlas_driver_offer_bpp")
