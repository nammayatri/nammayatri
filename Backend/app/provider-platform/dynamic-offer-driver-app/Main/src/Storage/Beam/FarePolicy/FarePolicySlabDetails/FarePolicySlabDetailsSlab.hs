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

module Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
-- import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Domain

import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import qualified EulerHS.Language as L
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import Lib.Utils
import Lib.UtilsTH
import Sequelize as Se
import qualified Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails as DomainFPPD
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

instance IsString Vehicle.Variant where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Money where
  fromString = show

instance FromField Domain.WaitingCharge where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.WaitingCharge where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.WaitingCharge

instance FromBackendRow Postgres Domain.WaitingCharge

instance FromField HighPrecMoney where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be HighPrecMoney where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

instance FromBackendRow Postgres HighPrecMoney

instance FromField Domain.NightShiftCharge where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.NightShiftCharge where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.NightShiftCharge

instance FromBackendRow Postgres Domain.NightShiftCharge

data FarePolicySlabsDetailsSlabT f = FarePolicySlabsDetailsSlabT
  { farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    baseFare :: B.C f Money,
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicySlabsDetailsSlabT where
  data PrimaryKey FarePolicySlabsDetailsSlabT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

instance ModelMeta FarePolicySlabsDetailsSlabT where
  modelFieldModification = farePolicySlabsDetailsSlabTMod
  modelTableName = "fare_parameters_progressive_details"
  mkExprWithDefault _ = B.insertExpressions []

type FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlabT Identity

instance FromJSON FarePolicySlabsDetailsSlab where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FarePolicySlabsDetailsSlab where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FarePolicySlabsDetailsSlab

deriving stock instance Ord Domain.WaitingCharge

deriving stock instance Ord Domain.NightShiftCharge

deriving stock instance Read Domain.NightShiftCharge

deriving stock instance Read Domain.WaitingCharge

farePolicySlabsDetailsSlabTMod :: FarePolicySlabsDetailsSlabT (B.FieldModification (B.TableField FarePolicySlabsDetailsSlabT))
farePolicySlabsDetailsSlabTMod =
  B.tableModification
    { farePolicyId = B.fieldNamed "fare_policy_id",
      startDistance = B.fieldNamed "start_distance",
      baseFare = B.fieldNamed "base_fare",
      waitingCharge = B.fieldNamed "waiting_charge",
      nightShiftCharge = B.fieldNamed "night_shift_charge"
    }

instance Serialize FarePolicySlabsDetailsSlab where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

farePolicySlabsDetailsSlabToHSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicySlabsDetailsSlabToHSModifiers =
  M.fromList
    []

farePolicySlabsDetailsSlabToPSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicySlabsDetailsSlabToPSModifiers =
  M.fromList
    []

$(enableKVPG ''FarePolicySlabsDetailsSlabT ['farePolicyId] [])
