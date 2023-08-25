{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.RegistrationToken where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data Medium
  = SMS
  | EMAIL
  | SIGNATURE
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Ord, Read, ToSchema)

instance FromField Medium where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Medium where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Medium

instance FromBackendRow Postgres Medium

instance IsString Medium where
  fromString = show

data RTEntityType
  = CUSTOMER
  | USER
  deriving (Generic, FromJSON, ToJSON, Eq, Ord, Show, Read)

instance FromField RTEntityType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RTEntityType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RTEntityType

instance FromBackendRow Postgres RTEntityType

instance IsString RTEntityType where
  fromString = show

data LoginType
  = OTP
  | PASSWORD
  | DIRECT
  deriving (Generic, FromJSON, ToJSON, Eq, Ord, Show, Read, ToSchema)

instance FromField LoginType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LoginType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be LoginType

instance FromBackendRow Postgres LoginType

instance IsString LoginType where
  fromString = show

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    attempts :: Int,
    authMedium :: Medium,
    authType :: LoginType,
    authValueHash :: Text,
    verified :: Bool,
    authExpiry :: Int,
    tokenExpiry :: Int,
    entityId :: Text,
    merchantId :: Text,
    entityType :: RTEntityType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    info :: Maybe Text
  }
  deriving (Generic, Show)
