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

module Storage.Beam.FarePolicy.RestrictedExtraFare where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

instance IsString Vehicle.Variant where
  fromString = show

instance IsString Money where
  fromString = show

data RestrictedExtraFareT f = RestrictedExtraFareT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    vehicleVariant :: B.C f Vehicle.Variant,
    minTripDistance :: B.C f Meters,
    driverMaxExtraFare :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table RestrictedExtraFareT where
  data PrimaryKey RestrictedExtraFareT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta RestrictedExtraFareT where
  modelFieldModification = restrictedExtraFareTMod
  modelTableName = "restricted_extra_fare"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type RestrictedExtraFare = RestrictedExtraFareT Identity

instance FromJSON RestrictedExtraFare where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON RestrictedExtraFare where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show RestrictedExtraFare

restrictedExtraFareTMod :: RestrictedExtraFareT (B.FieldModification (B.TableField RestrictedExtraFareT))
restrictedExtraFareTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      minTripDistance = B.fieldNamed "min_trip_distance",
      driverMaxExtraFare = B.fieldNamed "driver_max_extra_fare"
    }

defaultRestrictedExtraFare :: RestrictedExtraFare
defaultRestrictedExtraFare =
  RestrictedExtraFareT
    { id = "",
      merchantId = "",
      vehicleVariant = "",
      minTripDistance = "",
      driverMaxExtraFare = ""
    }

instance Serialize RestrictedExtraFare where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

restrictedExtraFareToHSModifiers :: M.Map Text (A.Value -> A.Value)
restrictedExtraFareToHSModifiers =
  M.empty

restrictedExtraFareToPSModifiers :: M.Map Text (A.Value -> A.Value)
restrictedExtraFareToPSModifiers =
  M.empty

$(enableKVPG ''RestrictedExtraFareT ['id] [])
