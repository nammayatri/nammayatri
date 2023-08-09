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

module Storage.Beam.FareParameters where

import qualified Data.Aeson as A
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.FareParameters as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance IsString Centesimal where
  fromString = show

instance FromBackendRow Postgres Domain.FareParametersType

instance FromField Domain.FareParametersType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.FareParametersType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FareParametersType

data FareParametersT f = FareParametersT
  { id :: B.C f Text,
    baseFare :: B.C f Money,
    driverSelectedFare :: B.C f (Maybe Money),
    customerExtraFee :: B.C f (Maybe Money),
    waitingCharge :: B.C f (Maybe Money),
    nightShiftCharge :: B.C f (Maybe Money),
    nightShiftRateIfApplies :: B.C f (Maybe Double),
    serviceCharge :: B.C f (Maybe Money),
    fareParametersType :: B.C f Domain.FareParametersType,
    govtCharges :: B.C f (Maybe Money)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersT where
  data PrimaryKey FareParametersT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FareParameters = FareParametersT Identity

instance FromJSON Domain.FareParametersType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.FareParametersType where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Ord Domain.FareParametersType

deriving stock instance Eq Domain.FareParametersType

fareParametersTMod :: FareParametersT (B.FieldModification (B.TableField FareParametersT))
fareParametersTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      baseFare = B.fieldNamed "base_fare",
      driverSelectedFare = B.fieldNamed "driver_selected_fare",
      customerExtraFee = B.fieldNamed "customer_extra_fee",
      nightShiftRateIfApplies = B.fieldNamed "night_shift_rate_if_applies",
      serviceCharge = B.fieldNamed "service_charge",
      fareParametersType = B.fieldNamed "fare_parameters_type",
      govtCharges = B.fieldNamed "govt_charges",
      waitingCharge = B.fieldNamed "waiting_charge",
      nightShiftCharge = B.fieldNamed "night_shift_charge"
    }

$(enableKVPG ''FareParametersT ['id] [])

$(mkTableInstances ''FareParametersT "fare_parameters" "atlas_driver_offer_bpp")
