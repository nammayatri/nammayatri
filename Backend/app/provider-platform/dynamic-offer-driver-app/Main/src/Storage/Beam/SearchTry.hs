{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SearchTry where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Domain.Types.SearchTry as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Sequelize
import Tools.Beam.UtilsTH

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

data SearchTryT f = SearchTryT
  { id :: B.C f Text,
    messageId :: B.C f Text,
    requestId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    startTime :: B.C f Time.UTCTime,
    validTill :: B.C f Time.UTCTime,
    estimateId :: B.C f Text,
    baseFare :: B.C f Money,
    customerExtraFee :: B.C f (Maybe Money),
    status :: B.C f Domain.SearchTryStatus,
    vehicleVariant :: B.C f Variant.Variant,
    searchRepeatCounter :: B.C f Int,
    searchRepeatType :: B.C f Domain.SearchRepeatType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchTryT where
  data PrimaryKey SearchTryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchTry = SearchTryT Identity

$(enableKVPG ''SearchTryT ['id] [['requestId]])

$(mkTableInstances ''SearchTryT "search_try")
