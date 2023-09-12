{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto (Point (..))
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH (enableKVPG, mkTableInstances)

toRowExpression reqId driverId lat lon status numCancellation mbReachedHome createdAt updatedAt =
  DriverGoHomeRequestT
    (B.val_ reqId)
    (B.val_ driverId)
    (B.val_ lat)
    (B.val_ lon)
    (getPoint (lat, lon))
    (B.val_ status)
    (B.val_ numCancellation)
    (B.val_ mbReachedHome)
    (B.val_ createdAt)
    (B.val_ updatedAt)

data DriverGoHomeRequestT f = DriverGoHomeRequestT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    point :: B.C f Point,
    status :: B.C f Domain.DriverGoHomeRequestStatus,
    numCancellation :: B.C f Int,
    reachedHome :: B.C f (Maybe Bool),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverGoHomeRequestT where
  data PrimaryKey DriverGoHomeRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverGoHomeRequest = DriverGoHomeRequestT Identity

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverGoHomeRequestToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverGoHomeRequestToHSModifiers =
  M.empty

driverGoHomeRequestToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverGoHomeRequestToPSModifiers =
  M.empty

$(enableKVPG ''DriverGoHomeRequestT ['id] [])

$(mkTableInstances ''DriverGoHomeRequestT "driver_go_home_request")
