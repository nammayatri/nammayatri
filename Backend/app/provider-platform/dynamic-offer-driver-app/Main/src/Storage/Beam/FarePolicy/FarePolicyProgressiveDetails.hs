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

module Storage.Beam.FarePolicy.FarePolicyProgressiveDetails where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize as Se

instance IsString Vehicle.Variant where
  fromString = show

data FarePolicyProgressiveDetailsT f = FarePolicyProgressiveDetailsT
  { farePolicyId :: B.C f Text,
    baseDistance :: B.C f Meters,
    baseFare :: B.C f Money,
    deadKmFare :: B.C f Money,
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsT where
  data PrimaryKey FarePolicyProgressiveDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

instance ModelMeta FarePolicyProgressiveDetailsT where
  modelFieldModification = farePolicyProgressiveDetailsTMod
  modelTableName = "fare_policy_progressive_details"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FarePolicyProgressiveDetails = FarePolicyProgressiveDetailsT Identity

instance FromJSON FarePolicyProgressiveDetails where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FarePolicyProgressiveDetails where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FarePolicyProgressiveDetails

deriving stock instance Ord Domain.WaitingCharge

deriving stock instance Ord Domain.NightShiftCharge

farePolicyProgressiveDetailsTMod :: FarePolicyProgressiveDetailsT (B.FieldModification (B.TableField FarePolicyProgressiveDetailsT))
farePolicyProgressiveDetailsTMod =
  B.tableModification
    { farePolicyId = B.fieldNamed "fare_policy_id",
      baseDistance = B.fieldNamed "base_distance",
      baseFare = B.fieldNamed "base_fare",
      deadKmFare = B.fieldNamed "dead_km_fare",
      waitingCharge = B.fieldNamed "waiting_charge",
      freeWatingTime = B.fieldNamed "free_wating_time",
      nightShiftCharge = B.fieldNamed "night_shift_charge"
    }

instance Serialize FarePolicyProgressiveDetails where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

farePolicyProgressiveDetailsToHSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicyProgressiveDetailsToHSModifiers =
  M.empty

farePolicyProgressiveDetailsToPSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicyProgressiveDetailsToPSModifiers =
  M.empty

$(enableKVPG ''FarePolicyProgressiveDetailsT ['farePolicyId] [])
