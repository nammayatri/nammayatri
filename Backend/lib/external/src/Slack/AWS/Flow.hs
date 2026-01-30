{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Slack.AWS.Flow where

import qualified Amazonka as AWS
import Amazonka.SNS.Publish
import qualified Data.Text as T
import Kernel.Prelude

publishMessage :: T.Text -> T.Text -> IO ()
publishMessage topicArn message = do
  putStrLn $ ("DEBUG: Slack.AWS.Flow: publishMessage called. TopicArn: " :: Text) <> topicArn
  putStrLn $ ("DEBUG: Slack.AWS.Flow: Message content: " :: Text) <> message
  env <- AWS.newEnv AWS.discover
  let pubReq =
        Publish'
          { messageAttributes = Nothing,
            messageDeduplicationId = Nothing,
            messageGroupId = Nothing,
            messageStructure = Nothing,
            phoneNumber = Nothing,
            subject = Nothing,
            targetArn = Nothing,
            topicArn = Just topicArn,
            ..
          }
  putStrLn ("DEBUG: Slack.AWS.Flow: Sending to AWS SNS..." :: Text)
  void $ AWS.runResourceT $ AWS.send env pubReq
  putStrLn ("DEBUG: Slack.AWS.Flow: Successfully sent to AWS SNS." :: Text)
