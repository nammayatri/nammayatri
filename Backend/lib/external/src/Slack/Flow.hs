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
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.App (lookupCloudType)
import qualified Slack.AWS.Flow as AWS
import qualified Slack.GCP.Flow as GCP
import Slack.Types

publishMessage :: SlackNotificationConfig -> T.Text -> IO ()
publishMessage config message = do
  putStrLn $ ("DEBUG: Slack.Flow: publishMessage called. Config: " :: Text) <> show config
  putStrLn $ ("DEBUG: Slack.Flow: Message content length: " :: Text) <> show (T.length message)

  if config.isForcedAWS
    then do
      putStrLn ("DEBUG: Slack.Flow: isForcedAWS is TRUE. Forcing routing to AWS SNS..." :: Text)
      routeToAWS config message
    else do
      putStrLn ("DEBUG: Slack.Flow: isForcedAWS is FALSE. Looking up CloudType..." :: Text)
      cloudType <- lookupCloudType
      putStrLn $ ("DEBUG: Slack.Flow: Detected CloudType: " :: Text) <> show cloudType
      case cloudType of
        AWS -> routeToAWS config message
        GCP -> routeToGCP config message
        UNAVAILABLE -> do
             putStrLn ("ERROR: Slack.Flow: CloudType UNAVAILABLE" :: Text)
             error "CloudType UNAVAILABLE: Cannot route slack message"

routeToAWS :: SlackNotificationConfig -> T.Text -> IO ()
routeToAWS config message = do
  putStrLn ("DEBUG: Slack.Flow: Routing to AWS SNS..." :: Text)
  case config.snsTopicArn of
    Just topicArn -> AWS.publishMessage topicArn message
    Nothing -> do
         putStrLn ("ERROR: Slack.Flow: AWS SNS Topic ARN not configured!" :: Text)
         error "AWS SNS Topic ARN not configured in slackNotificationConfig"

routeToGCP :: SlackNotificationConfig -> T.Text -> IO ()
routeToGCP config message = do
  putStrLn ("DEBUG: Slack.Flow: Routing to GCP Pub/Sub..." :: Text)
  case (config.gcpProjectId, config.gcpTopicId) of
    (Just projectId, Just topicId) -> do
         putStrLn $ ("DEBUG: Slack.Flow: Calling GCP.publishMessage with ProjectId=" :: Text) <> projectId <> ", TopicId=" <> topicId
         GCP.publishMessage projectId topicId message
    _ -> do
         putStrLn ("ERROR: Slack.Flow: GCP Pub/Sub project/topic not configured!" :: Text)
         error "GCP Pub/Sub project/topic not configured in slackNotificationConfig"
