{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Storage.Beam.DriverFee where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.DriverFee as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Prelude (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

data DriverFeeT f = DriverFeeT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    driverId :: B.C f Text,
    totalEarnings :: B.C f Money,
    govtCharges :: B.C f Money,
    platformFee :: B.C f Money,
    cgst :: B.C f HighPrecMoney,
    sgst :: B.C f HighPrecMoney,
    payBy :: B.C f Time.UTCTime,
    startTime :: B.C f Time.UTCTime,
    endTime :: B.C f Time.UTCTime,
    numRides :: B.C f Int,
    status :: B.C f Domain.DriverFeeStatus,
    collectedBy :: B.C f (Maybe Text),
    feeType :: B.C f Domain.FeeType,
    offerId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFeeT where
  data PrimaryKey DriverFeeT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverFee = DriverFeeT Identity

driverFeeTMod :: DriverFeeT (B.FieldModification (B.TableField DriverFeeT))
driverFeeTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      driverId = B.fieldNamed "driver_id",
      totalEarnings = B.fieldNamed "total_earnings",
      govtCharges = B.fieldNamed "govt_charges",
      platformFee = B.fieldNamed "platform_fee",
      cgst = B.fieldNamed "cgst",
      sgst = B.fieldNamed "sgst",
      payBy = B.fieldNamed "pay_by",
      startTime = B.fieldNamed "start_time",
      endTime = B.fieldNamed "end_time",
      numRides = B.fieldNamed "num_rides",
      status = B.fieldNamed "status",
      collectedBy = B.fieldNamed "collected_by",
      feeType = B.fieldNamed "fee_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverFeeT ['id] [['driverId]])

$(mkTableInstances ''DriverFeeT "driver_fee" "atlas_driver_offer_bpp")
