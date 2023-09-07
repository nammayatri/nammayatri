{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Issue.IssueReport where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Issue.IssueCategory as D
import qualified Domain.Types.Issue.IssueOption as D
import qualified Domain.Types.MediaFile as D
import qualified Domain.Types.Person as D
import qualified Domain.Types.Ride as D
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as BP
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data IssueStatus
  = OPEN
  | PENDING
  | RESOLVED
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''IssueStatus)

data IssueReport = IssueReport
  { id :: Id IssueReport,
    driverId :: Id D.Person,
    rideId :: Maybe (Id D.Ride),
    description :: Text,
    assignee :: Maybe Text,
    status :: IssueStatus,
    categoryId :: Id D.IssueCategory,
    optionId :: Maybe (Id D.IssueOption),
    deleted :: Bool,
    mediaFiles :: [Id D.MediaFile],
    ticketId :: Maybe Text, -- third party id
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
