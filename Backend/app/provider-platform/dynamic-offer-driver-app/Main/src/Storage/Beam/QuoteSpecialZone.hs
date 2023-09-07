{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.QuoteSpecialZone where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Common as Common
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data QuoteSpecialZoneT f = QuoteSpecialZoneT
  { id :: B.C f Text,
    searchRequestId :: B.C f Text,
    providerId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    distance :: B.C f Meters,
    validTill :: B.C f Time.LocalTime,
    estimatedFare :: B.C f Common.Money,
    fareParametersId :: B.C f Text,
    estimatedFinishTime :: B.C f Time.UTCTime,
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteSpecialZoneT where
  data PrimaryKey QuoteSpecialZoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type QuoteSpecialZone = QuoteSpecialZoneT Identity

$(enableKVPG ''QuoteSpecialZoneT ['id] [['searchRequestId]])

$(mkTableInstances ''QuoteSpecialZoneT "quote_special_zone")
