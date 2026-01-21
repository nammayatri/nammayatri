{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Slack.Flow
  ( publishMessage,
    module Slack.Types,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import qualified Slack.AWS.Flow as AWS
import qualified Slack.GCP.Flow as GCP
import Slack.Types


import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.App (lookupCloudType)

publishMessage :: SlackNotificationConfig -> T.Text -> IO ()
publishMessage config message = do
  cloudType <- lookupCloudType
  case cloudType of
    AWS -> case config.snsTopicArn of
      Just topicArn -> AWS.publishMessage topicArn message
      Nothing -> error "AWS SNS Topic ARN not configured in slackNotificationConfig"
    GCP -> case (config.gcpProjectId, config.gcpTopicId) of
      (Just projectId, Just topicId) -> GCP.publishMessage projectId topicId message
      _ -> error "GCP Pub/Sub project/topic not configured in slackNotificationConfig"
    UNAVAILABLE -> error "CloudType UNAVAILABLE: Cannot route slack message"
