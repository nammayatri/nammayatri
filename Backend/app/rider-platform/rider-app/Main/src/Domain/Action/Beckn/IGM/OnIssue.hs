{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.IGM.OnIssue where

import qualified Domain.Types.IGMIssue as DIGM
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IGMIssue as QIGM

data DOnIssue = DOnIssue
  { id :: Text,
    providerId :: Text,
    providerUrl :: Text,
    respondentName :: Maybe Text,
    respondentEmail :: Maybe Text,
    respondentPhone :: Maybe Text,
    respondentAction :: Maybe Text,
    updatedAt :: Maybe UTCTime
  }

validateRequest :: DOnIssue -> Flow (DIGM.IGMIssue)
validateRequest igmIssue = QIGM.findByPrimaryKey (Id igmIssue.id) >>= fromMaybeM (InvalidRequest "Issue not found")

handler ::
  DOnIssue ->
  DIGM.IGMIssue ->
  Flow ()
handler onIssueReq issue = do
  now <- getCurrentTime
  let issueStatus = mapActionToStatus onIssueReq.respondentAction & fromMaybe issue.issueStatus
  let updatedIssue =
        issue
          { DIGM.respondentName = onIssueReq.respondentName,
            DIGM.respondentEmail = onIssueReq.respondentEmail,
            DIGM.respondentPhone = onIssueReq.respondentPhone,
            DIGM.respondentAction = onIssueReq.respondentAction,
            DIGM.updatedAt = onIssueReq.updatedAt & fromMaybe now,
            DIGM.issueStatus = issueStatus
          }
  QIGM.updateByPrimaryKey updatedIssue
  pure ()

mapActionToStatus :: Maybe Text -> Maybe DIGM.Status
mapActionToStatus (Just "RESOLVED") = Just DIGM.RESOLVED
mapActionToStatus _ = Nothing
