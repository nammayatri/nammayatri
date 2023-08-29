{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverReferral where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data DriverReferralT f = DriverReferralT
  { referralCode :: B.C f Text,
    driverId :: B.C f Text,
    linkedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverReferralT where
  data PrimaryKey DriverReferralT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . referralCode

type DriverReferral = DriverReferralT Identity

driverReferralTMod :: DriverReferralT (B.FieldModification (B.TableField DriverReferralT))
driverReferralTMod =
  B.tableModification
    { referralCode = B.fieldNamed "referral_code",
      driverId = B.fieldNamed "driver_id",
      linkedAt = B.fieldNamed "linked_at"
    }

$(enableKVPG ''DriverReferralT ['referralCode] [['driverId]])

$(mkTableInstances ''DriverReferralT "driver_referral" "atlas_driver_offer_bpp")
