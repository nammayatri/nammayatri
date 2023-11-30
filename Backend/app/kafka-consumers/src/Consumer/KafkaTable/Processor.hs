{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.KafkaTable.Processor
  ( kafkaTableProcessor,
  )
where

import qualified AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Environment
import Kernel.Prelude
import Kernel.Streaming.Kafka.KafkaTable as Kafka
import Kernel.Utils.Common (encodeToText, logDebug, logWarning)

kafkaTableProcessor :: Map.Map String [Kafka.KafkaTable] -> UTCTime -> Flow ()
kafkaTableProcessor mapKafkaTable now = do
  pathPrefix <- T.unpack <$> asks (.s3Env.pathPrefix)
  void $
    flip Map.traverseWithKey mapKafkaTable $ \filePathWithoutPrefix mappedKafkaTables -> do
      let filePath = pathPrefix <> "/" <> filePathWithoutPrefix
      logDebug $ "MessagesReceived: " <> show (length mappedKafkaTables) <> "; filePath: " <> show filePath
      existingTableContent <- handle @Flow @SomeException (pure . const "") (S3.get filePath)
      let newTableContent = T.concat ((<> "\n") . (encodeToText @A.Value) . (.tableContent) <$> mappedKafkaTables)
      if T.null existingTableContent
        then do
          -- normal case
          logDebug $ "Create new file: " <> show filePath
          S3.put filePath newTableContent
        else do
          -- creating new file because of some drainer delay while appending current time of consumer to the file name
          let newFilePath = filePath <> "_consumedAt_" <> show now
          logWarning $ "Creating new file for existing table " <> show filePath
          S3.put newFilePath newTableContent
