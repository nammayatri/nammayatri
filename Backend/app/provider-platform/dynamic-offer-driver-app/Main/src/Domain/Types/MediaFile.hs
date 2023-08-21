{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.MediaFile where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data MediaType = Video | Audio | Image | AudioLink | VideoLink | ImageLink | PortraitVideoLink deriving (Read, Show, Generic, ToSchema, ToJSON, FromJSON)

instance FromField MediaType where
  fromField = fromFieldEnum

deriving stock instance Ord MediaType

deriving stock instance Eq MediaType

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MediaType

instance FromBackendRow Postgres MediaType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MediaType where
  sqlValueSyntax = autoSqlValueSyntax

instance IsString MediaType where
  fromString = show

data MediaFile = MediaFile
  { id :: Id MediaFile,
    _type :: MediaType,
    url :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON)
