{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Issue where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data IssueStatus
  = OPEN
  | PENDING
  | RESOLVED
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON)

instance FromField IssueStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IssueStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be IssueStatus

instance FromBackendRow Postgres IssueStatus

instance IsString IssueStatus where
  fromString = show

data Issue = Issue
  { id :: Id Issue,
    customerId :: Id DPerson.Person,
    bookingId :: Maybe (Id DQuote.Quote),
    contactEmail :: Maybe Text,
    reason :: Text,
    description :: Text,
    ticketId :: Maybe Text,
    status :: IssueStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
