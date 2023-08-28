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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.CallStatus where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Call.CallStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Call.CallStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Call.CallStatus

instance FromBackendRow Postgres Call.CallStatus

deriving stock instance Ord Call.CallStatus

instance IsString Call.CallStatus where
  fromString = show

data CallStatusT f = CallStatusT
  { id :: B.C f Text,
    callId :: B.C f Text,
    entityId :: B.C f Text,
    dtmfNumberUsed :: B.C f (Maybe Text),
    status :: B.C f Call.CallStatus,
    recordingUrl :: B.C f (Maybe Text),
    conversationDuration :: B.C f Int,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CallStatusT where
  data PrimaryKey CallStatusT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type CallStatus = CallStatusT Identity

callStatusTMod :: CallStatusT (B.FieldModification (B.TableField CallStatusT))
callStatusTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      callId = B.fieldNamed "call_id",
      entityId = B.fieldNamed "entity_id",
      dtmfNumberUsed = B.fieldNamed "dtmf_number_used",
      status = B.fieldNamed "status",
      recordingUrl = B.fieldNamed "recording_url",
      conversationDuration = B.fieldNamed "conversation_duration",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''CallStatusT ['id] [['callId]])

$(mkTableInstances ''CallStatusT "call_status" "atlas_driver_offer_bpp")
