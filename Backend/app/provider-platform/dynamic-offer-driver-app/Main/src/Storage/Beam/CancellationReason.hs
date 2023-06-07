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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.CancellationReason where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize as Se

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError DPSF.ConversionFailed f "Could not 'read' value for 'Rule'."

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

instance ModelMeta CancellationReasonT where
  modelFieldModification = cancellationReasonTMod
  modelTableName = "cancellation_reason"
  mkExprWithDefault _ = B.insertExpressions []
  modelSchemaName = Just "atlas_driver_offer_bpp"

type CancellationReason = CancellationReasonT Identity

instance FromJSON CancellationReason where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON CancellationReason where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show CancellationReason

cancellationReasonTMod :: CancellationReasonT (B.FieldModification (B.TableField CancellationReasonT))
cancellationReasonTMod =
  B.tableModification
    { reasonCode = B.fieldNamed "reason_code",
      description = B.fieldNamed "description",
      enabled = B.fieldNamed "enabled",
      priority = B.fieldNamed "priority"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

cancellationReasonToHSModifiers :: M.Map Text (A.Value -> A.Value)
cancellationReasonToHSModifiers =
  M.empty

cancellationReasonToPSModifiers :: M.Map Text (A.Value -> A.Value)
cancellationReasonToPSModifiers =
  M.empty

instance Serialize CancellationReason where
  put = error "undefined"
  get = error "undefined"

defaultCancellationReason :: CancellationReason
defaultCancellationReason =
  CancellationReasonT
    { reasonCode = "",
      description = "",
      enabled = False,
      priority = 0
    }

$(enableKVPG ''CancellationReasonT ['reasonCode] [])
