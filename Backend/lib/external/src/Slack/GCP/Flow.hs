{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Slack.GCP.Flow
  ( publishMessage,
  )
where

import qualified Data.Text as T
import qualified Google.Cloud.PubSub.Topic as PubSub
import Kernel.Prelude

publishMessage :: T.Text -> T.Text -> T.Text -> IO ()
publishMessage projectId topicId message = do
  let msg = PubSub.Message (T.unpack message) Nothing Nothing
  result <- PubSub.publishMessage (T.unpack projectId) (T.unpack topicId) [msg]
  case result of
    Left err -> error $ "Failed to publish message: " <> T.pack err
    Right _ -> return ()
