{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.IGM.OnIssueStatus where

import Control.Applicative
import qualified Domain.Types.IGMIssue as DIGM
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.IGMIssue as QIGM

data DOnIssueStatus = DOnIssueStatus
  { id :: Text,
    providerId :: Text,
    respondentName :: Maybe Text,
    respondentEmail :: Maybe Text,
    respondentPhone :: Maybe Text,
    respondentAction :: Maybe Text,
    resolutionActionTriggered :: Maybe Text,
    updatedAt :: UTCTime
  }

validateRequest :: DOnIssueStatus -> Flow (DIGM.IGMIssue)
validateRequest req = QIGM.findByPrimaryKey (Id $ req.id) >>= fromMaybeM (InvalidRequest "Issue not found")

onIssueStatus :: DOnIssueStatus -> DIGM.IGMIssue -> Flow ()
onIssueStatus req issue = do
  let updatedIssue =
        issue
          { DIGM.respondentName = req.respondentName <|> issue.respondentName,
            DIGM.respondentEmail = req.respondentEmail <|> issue.respondentEmail,
            DIGM.respondentPhone = req.respondentPhone <|> issue.respondentPhone,
            DIGM.respondentAction = req.respondentAction <|> issue.respondentAction,
            DIGM.issueStatus = mapActionToStatus req.respondentAction & fromMaybe issue.issueStatus,
            DIGM.updatedAt = req.updatedAt
          }
  QIGM.updateByPrimaryKey updatedIssue
  pure ()

mapActionToStatus :: Maybe Text -> Maybe DIGM.Status
mapActionToStatus (Just "RESOLVED") = Just DIGM.RESOLVED
mapActionToStatus _ = Nothing
