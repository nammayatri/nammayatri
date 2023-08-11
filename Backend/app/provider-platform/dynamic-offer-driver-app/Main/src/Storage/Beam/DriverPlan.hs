{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.DriverPlan where

import qualified Domain.Types.DriverPlan as Domain
import Domain.Types.Person (Person)
import qualified Domain.Types.Plan as DPlan
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
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
import EulerHS.Prelude (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data DriverPlanT f = DriverPlanT
    {
        driverId :: B.C f Text,
        planId :: B.C f Text,
        planType :: B.C f DPlan.PaymentMode,
        mandateId :: B.C f Text Maybe,
        createdAt :: B.C f UTCTime,
        updatedAt :: B.C f UTCTime
    }
    deriving(Generic, B.Beamable)


instance B.Table DriverPlanT where
  data PrimaryKey DriverPlanT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta DriverPlanT where
  modelFieldModification = driverPlanTMod
  modelTableName = "driver_plan"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverPlan = DriverPlanT Identity

instance FromJSON DriverPlan where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverPlan where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverPlan

driverPlanTMod :: DriverPlanT (B.FieldModification (B.TableField DriverPlanT))
driverPlanTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driverId",
      planId = B.fieldNamed "planId",
      planType = B.fieldNamed "planType",
      mandateId = B.fieldNamed "mandateId",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize DriverPlan where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverFeeToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverFeeToHSModifiers =
  M.empty

driverFeeToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverFeeToPSModifiers =
  M.empty

$(enableKVPG ''DriverPlanT ['driverId])