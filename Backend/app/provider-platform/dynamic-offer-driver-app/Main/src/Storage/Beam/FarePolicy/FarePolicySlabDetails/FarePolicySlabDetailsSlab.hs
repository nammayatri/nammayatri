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
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend ()
import Database.Beam.MySQL ()
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize as Se

-- import Storage.Tabular.Vehicle ()

instance IsString Vehicle.Variant where
  fromString = show

data FarePolicySlabsDetailsSlabT f = FarePolicySlabsDetailsSlabT
  { id :: B.C f (Maybe Int),
    farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    baseFare :: B.C f Money,
    platformFeeCharge :: B.C f (Maybe Domain.PlatformFeeCharge),
    platformFeeCgst :: B.C f (Maybe Double),
    platformFeeSgst :: B.C f (Maybe Double),
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicySlabsDetailsSlabT where
  data PrimaryKey FarePolicySlabsDetailsSlabT f
    = Id (B.C f (Maybe Int))
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FarePolicySlabsDetailsSlabT where
  modelFieldModification = farePolicySlabsDetailsSlabTMod
  modelTableName = "fare_policy_slabs_details_slab"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlabT Identity

type FullFarePolicySlabsDetailsSlab = (KTI.Id Domain.FarePolicy, Domain.FPSlabsDetailsSlab)

instance FromJSON FarePolicySlabsDetailsSlab where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FarePolicySlabsDetailsSlab where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FarePolicySlabsDetailsSlab

deriving stock instance Ord Domain.WaitingCharge

deriving stock instance Ord Domain.WaitingChargeInfo

deriving stock instance Ord Domain.NightShiftCharge

deriving stock instance Ord Domain.PlatformFeeCharge

farePolicySlabsDetailsSlabTMod :: FarePolicySlabsDetailsSlabT (B.FieldModification (B.TableField FarePolicySlabsDetailsSlabT))
farePolicySlabsDetailsSlabTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      farePolicyId = B.fieldNamed "fare_policy_id",
      startDistance = B.fieldNamed "start_distance",
      baseFare = B.fieldNamed "base_fare",
      platformFeeCharge = B.fieldNamed "platform_fee_charge",
      platformFeeCgst = B.fieldNamed "platform_fee_cgst",
      platformFeeSgst = B.fieldNamed "platform_fee_sgst",
      freeWatingTime = B.fieldNamed "free_wating_time",
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
  M.empty

farePolicySlabsDetailsSlabToPSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicySlabsDetailsSlabToPSModifiers =
  M.empty

$(enableKVPG ''FarePolicySlabsDetailsSlabT ['id] [['farePolicyId]])
