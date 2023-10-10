{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.BecknRequest.Processor
  ( becknRequestProcessor,
    BecknRequestType (..),
  )
where

import qualified AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Beam.BecknRequest as BR
import Kernel.Types.Error
import Kernel.Utils.Common (decodeFromText, encodeToText, logDebug, logWarning, throwError)

data BecknRequestType = RIDER | DRIVER

becknRequestProcessor :: BecknRequestType -> [BR.BecknRequestKafka] -> Flow ()
becknRequestProcessor becknRequestType becknRequestsKafka = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  let mapBecknRequestsKafka = foldr (foldFunc pathPrefix) Map.empty becknRequestsKafka
  void $
    flip Map.traverseWithKey mapBecknRequestsKafka $ \filePath mappedBecknRequestKafka -> do
      logDebug $ "MessagesReceived: " <> show (length mappedBecknRequestKafka) <> "; filePath: " <> show filePath
      existingFile <- handle @Flow @SomeException (pure . const "") (S3.get filePath)
      if T.null existingFile
        then do
          logDebug $ "Create new file: " <> show filePath
          S3.put filePath (encodeToText @[A.Value] (mappedBecknRequestKafka <&> (.becknRequest))) -- normal case
        else do
          let mbExistingRequests :: Maybe [A.Value] = decodeFromText existingFile
          case mbExistingRequests of
            Just existingBecknRequests -> do
              -- overwriting file because of some drainer delay
              logWarning $ "Overwriting file with beckn requests: " <> show filePath
              S3.put filePath (encodeToText @[A.Value] $ existingBecknRequests <> (mappedBecknRequestKafka <&> (.becknRequest)))
            Nothing -> do
              -- should never happen
              throwError (InternalError $ "Could not decode existing file: " <> show filePath)
  where
    foldFunc ::
      Text ->
      BR.BecknRequestKafka ->
      Map.Map String [BR.BecknRequestKafka] ->
      Map.Map String [BR.BecknRequestKafka]
    foldFunc pathPrefix becknRequest mapBecknRequest = do
      let filePath = mkFilePath becknRequestType pathPrefix becknRequest.timestamp
      Map.insertWithKey (\_key newList oldList -> newList <> oldList) filePath [becknRequest] mapBecknRequest

mkFilePath :: BecknRequestType -> Text -> UTCTime -> String
mkFilePath becknRequestType pathPrefix timestamp = do
  let folderName = case becknRequestType of
        RIDER -> "rider_beckn_requests"
        DRIVER -> "driver_beckn_requests"
  T.unpack pathPrefix
    <> "/"
    <> folderName
    <> "/"
    <> Time.formatTime Time.defaultTimeLocale "%Y.%m.%d-%H" timestamp
    <> ".json"
