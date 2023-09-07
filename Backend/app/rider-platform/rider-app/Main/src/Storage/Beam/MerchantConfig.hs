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
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.MerchantConfig where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data MerchantConfigT f = MerchantConfigT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    fraudBookingCancellationCountThreshold :: B.C f Int,
    fraudBookingCancellationCountWindow :: B.C f SWC.SlidingWindowOptions,
    fraudBookingTotalCountThreshold :: B.C f Int,
    fraudBookingCancelledByDriverCountThreshold :: B.C f Int,
    fraudBookingCancelledByDriverCountWindow :: B.C f SWC.SlidingWindowOptions,
    fraudSearchCountThreshold :: B.C f Int,
    fraudSearchCountWindow :: B.C f SWC.SlidingWindowOptions,
    fraudRideCountThreshold :: B.C f Int,
    fraudRideCountWindow :: B.C f SWC.SlidingWindowOptions,
    enabled :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigT where
  data PrimaryKey MerchantConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantConfig = MerchantConfigT Identity

$(enableKVPG ''MerchantConfigT ['id] [['merchantId]])

$(mkTableInstances ''MerchantConfigT "merchant_config")
