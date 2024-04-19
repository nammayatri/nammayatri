{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.IGM.Issue where

import Domain.Types.Booking
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.Booking as QB

data DIssue = DIssue
  { issueId :: Text,
    issueCategory :: Text,
    issueType :: Text,
    issueStatus :: Text,
    bookingId :: Text,
    issueRaisedBy :: Maybe Text,
    customerName :: Maybe Text,
    customerEmail :: Maybe Text,
    customerPhone :: Maybe Text,
    bapId :: Text
  }
  deriving (Show, Eq, Generic)

data ValidatedDIssue = ValidatedDIssue
  { issueId :: Text,
    issueCategory :: Text,
    issueType :: Text,
    issueStatus :: Text,
    booking :: Booking,
    issueRaisedBy :: Maybe Text,
    customerName :: Maybe Text,
    customerEmail :: Maybe Text,
    customerPhone :: Maybe Text,
    bapId :: Text
  }

validateRequest :: DIssue -> Flow (ValidatedDIssue)
validateRequest dIssue@DIssue {..} = do
  booking <- QB.findById (Id dIssue.bookingId) >>= fromMaybeM (BookingDoesNotExist dIssue.bookingId)
  pure $ ValidatedDIssue {..}

issue :: ValidatedDIssue -> Flow ()
issue _ = pure () -- shrey00 : Implement this
