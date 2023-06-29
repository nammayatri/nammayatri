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

module Storage.Beam.FareParameters.FareParametersSlabDetails where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend ()
import Database.Beam.MySQL ()
import qualified Domain.Types.FareParameters as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KId
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize as Se
import Storage.Tabular.Vehicle ()

instance IsString Vehicle.Variant where
  fromString = show

data FareParametersSlabDetailsT f = FareParametersSlabDetailsT
  { fareParametersId :: B.C f Text,
    platformFee :: B.C f (Maybe Money),
    sgst :: B.C f (Maybe HighPrecMoney),
    cgst :: B.C f (Maybe HighPrecMoney)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersSlabDetailsT where
  data PrimaryKey FareParametersSlabDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . fareParametersId

instance ModelMeta FareParametersSlabDetailsT where
  modelFieldModification = fareParametersSlabDetailsTMod
  modelTableName = "fare_parameters_slab_details"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FareParametersSlabDetails = FareParametersSlabDetailsT Identity

type FullFareParametersSlabDetails = (KId.Id Domain.FareParameters, Domain.FParamsSlabDetails)

instance FromJSON FareParametersSlabDetails where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FareParametersSlabDetails where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FareParametersSlabDetails

fareParametersSlabDetailsTMod :: FareParametersSlabDetailsT (B.FieldModification (B.TableField FareParametersSlabDetailsT))
fareParametersSlabDetailsTMod =
  B.tableModification
    { fareParametersId = B.fieldNamed "fare_parameters_id",
      platformFee = B.fieldNamed "platform_fee",
      sgst = B.fieldNamed "sgst",
      cgst = B.fieldNamed "cgst"
    }

instance Serialize FareParametersSlabDetails where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

fareParametersSlabDetailsToHSModifiers :: M.Map Text (A.Value -> A.Value)
fareParametersSlabDetailsToHSModifiers =
  M.empty

fareParametersSlabDetailsToPSModifiers :: M.Map Text (A.Value -> A.Value)
fareParametersSlabDetailsToPSModifiers =
  M.empty

$(enableKVPG ''FareParametersSlabDetailsT ['fareParametersId] [])
