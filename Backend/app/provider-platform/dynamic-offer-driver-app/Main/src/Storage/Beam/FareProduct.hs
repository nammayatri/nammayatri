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

module Storage.Beam.FareProduct where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
-- import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Domain.FlowType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.FlowType where
  sqlValueSyntax = autoSqlValueSyntax

data FareProductT f = FareProductT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    farePolicyId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    area :: B.C f Domain.Area,
    flow :: B.C f Domain.FlowType
  }
  deriving (Generic, B.Beamable)

-- instance IsString Money where
--   fromString = show

instance B.Table FareProductT where
  data PrimaryKey FareProductT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FareProductT where
  modelFieldModification = fareProductTMod
  modelTableName = "fare_product"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FareProduct = FareProductT Identity

instance FromJSON FareProduct where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FareProduct where
  toJSON = A.genericToJSON A.defaultOptions

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FlowType

instance FromBackendRow Postgres Domain.FlowType

deriving stock instance Show FareProduct

fareProductTMod :: FareProductT (B.FieldModification (B.TableField FareProductT))
fareProductTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      farePolicyId = B.fieldNamed "fare_policy_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      area = B.fieldNamed "area",
      flow = B.fieldNamed "flow"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

fareProductToHSModifiers :: M.Map Text (A.Value -> A.Value)
fareProductToHSModifiers =
  M.empty

fareProductToPSModifiers :: M.Map Text (A.Value -> A.Value)
fareProductToPSModifiers =
  M.empty

instance Serialize FareProduct where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''FareProductT ['id] [])
