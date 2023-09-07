{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FareProduct where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data FareProductT f = FareProductT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    farePolicyId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    area :: B.C f Domain.Area,
    flow :: B.C f Domain.FlowType
  }
  deriving (Generic, B.Beamable)

instance B.Table FareProductT where
  data PrimaryKey FareProductT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FareProduct = FareProductT Identity

$(enableKVPG ''FareProductT ['id] [['merchantId, 'area]])

$(mkTableInstances ''FareProductT "fare_product")
