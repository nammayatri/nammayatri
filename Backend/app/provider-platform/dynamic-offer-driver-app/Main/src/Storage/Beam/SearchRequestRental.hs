{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SearchRequestRental where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Domain.Types.FareProduct as FareProductD
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax :: HasSqlValueSyntax be String => BaseUrl -> be
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

data SearchRequestRentalT f = SearchRequestRentalT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    messageId :: B.C f Text,
    startTime :: B.C f Time.UTCTime,
    validTill :: B.C f Time.UTCTime,
    providerId :: B.C f Text,
    fromLocationId :: B.C f Text,
    area :: B.C f (Maybe FareProductD.Area),
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestRentalT where
  data PrimaryKey SearchRequestRentalT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequestRental = SearchRequestRentalT Identity

$(enableKVPG ''SearchRequestRentalT ['id] [['transactionId], ['messageId]])

$(mkTableInstances ''SearchRequestRentalT "search_request_rental")
