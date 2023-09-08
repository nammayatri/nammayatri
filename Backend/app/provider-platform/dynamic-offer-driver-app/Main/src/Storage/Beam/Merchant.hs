{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Merchant where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Lib.Utils ()
import Sequelize

data MerchantT f = MerchantT
  { id :: B.C f Text,
    name :: B.C f Text,
    description :: B.C f (Maybe Text),
    subscriberId :: B.C f Text,
    uniqueKeyId :: B.C f Text,
    shortId :: B.C f Text,
    status :: B.C f Domain.Status,
    enabled :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Merchant = MerchantT Identity

merchantTMod :: MerchantT (B.FieldModification (B.TableField MerchantT))
merchantTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      name = B.fieldNamed "name",
      description = B.fieldNamed "description",
      subscriberId = B.fieldNamed "subscriber_id",
      uniqueKeyId = B.fieldNamed "unique_key_id",
      shortId = B.fieldNamed "short_id",
      status = B.fieldNamed "status",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''MerchantT ['id] [['subscriberId], ['shortId], ['status]])

$(mkTableInstances ''MerchantT "merchant" "atlas_driver_offer_bpp")
