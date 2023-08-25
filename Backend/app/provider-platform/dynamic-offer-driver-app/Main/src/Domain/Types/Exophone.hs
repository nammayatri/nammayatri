{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Exophone where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data Exophone = Exophone
  { id :: Id Exophone,
    merchantId :: Id DM.Merchant,
    primaryPhone :: Text,
    backupPhone :: Text,
    isPrimaryDown :: Bool,
    exophoneType :: ExophoneType,
    -- isBackupDown Bool -- do we need this?
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data ExophoneType = CALL_RIDE | END_RIDE
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromField ExophoneType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ExophoneType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be ExophoneType

instance FromBackendRow Postgres ExophoneType
