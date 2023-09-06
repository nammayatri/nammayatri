{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Message.MessageReport where

import Data.Aeson
import qualified Data.Aeson as A
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Message.MessageReport as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data MessageReportT f = MessageReportT
  { messageId :: B.C f Text,
    driverId :: B.C f Text,
    deliveryStatus :: B.C f Domain.DeliveryStatus,
    readStatus :: B.C f Bool,
    likeStatus :: B.C f Bool,
    reply :: B.C f (Maybe Text),
    messageDynamicFields :: B.C f A.Value,
    updatedAt :: B.C f Time.LocalTime,
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageReportT where
  data PrimaryKey MessageReportT f
    = Id (B.C f Text) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id <$> driverId <*> messageId

type MessageReport = MessageReportT Identity

messageReportTMod :: MessageReportT (B.FieldModification (B.TableField MessageReportT))
messageReportTMod =
  B.tableModification
    { messageId = B.fieldNamed "message_id",
      driverId = B.fieldNamed "driver_id",
      deliveryStatus = B.fieldNamed "delivery_status",
      readStatus = B.fieldNamed "read_status",
      likeStatus = B.fieldNamed "like_status",
      reply = B.fieldNamed "reply",
      messageDynamicFields = B.fieldNamed "message_dynamic_fields",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MessageReportT ['driverId, 'messageId] [])
$(mkTableInstances ''MessageReportT "message_report" "atlas_driver_offer_bpp")
