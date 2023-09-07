{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
  ( DriverPoolBatchesConfig (..),
    BatchSplitByPickupDistance (..),
    HasDriverPoolBatchesConfig,
    PoolSortingType (..),
  )
where

import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data BatchSplitByPickupDistance = BatchSplitByPickupDistance
  { batchSplitSize :: Int,
    batchSplitDelay :: Seconds
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromField BatchSplitByPickupDistance where
  fromField = fromFieldEnum

instance FromField [BatchSplitByPickupDistance] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [BatchSplitByPickupDistance] where
  sqlValueSyntax batchList =
    let x = (show <$> batchList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [BatchSplitByPickupDistance]

instance FromBackendRow Postgres [BatchSplitByPickupDistance]

data DriverPoolBatchesConfig = DriverPoolBatchesConfig
  { driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    poolSortingType :: PoolSortingType
  }
  deriving (Generic, FromDhall)

type HasDriverPoolBatchesConfig r =
  ( HasField "driverPoolBatchesCfg" r DriverPoolBatchesConfig
  )

data PoolSortingType = Intelligent | Random
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, FromDhall)

$(mkBeamInstancesForEnum ''PoolSortingType)
