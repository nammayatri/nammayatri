{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Message
  ( module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.Message
import Dashboard.Common as Reexport
import Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 as BLC
import Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.ServantMultipart

---
-- Upload File
--

instance FromMultipart Tmp UploadFileRequest where
  fromMultipart form = do
    file <- fmap fdPayload (lookupFile "file" form)
    reqContentType <- fmap fdFileCType (lookupFile "file" form)
    fileType <- fmap (read . T.unpack) (lookupInput "fileType" form)
    pure UploadFileRequest {..}

instance ToMultipart Tmp UploadFileRequest where
  toMultipart uploadFileRequest =
    MultipartData
      [Input "fileType" (show uploadFileRequest.fileType)]
      [FileData "file" (T.pack uploadFileRequest.file) "" (uploadFileRequest.file)]

instance FromMultipart Tmp SendMessageRequest where
  fromMultipart form = do
    let inputType = fmap (read . T.unpack) (lookupInput "type" form)
    csvFile <- either (helper inputType) (Right . Just . fdPayload) (lookupFile "csvFile" form)
    _type <- inputType
    messageId <- lookupInput "messageId" form
    let scheduledT = (A.eitherDecode . LBS.fromStrict . TE.encodeUtf8) =<< lookupInput "scheduledTime" form
    scheduledTime <- timeHelper scheduledT
    pure SendMessageRequest {..}
    where
      helper (Right AllEnabled) _ = Right Nothing
      helper _ x = Left x
      timeHelper (Left _) = Right Nothing
      timeHelper (Right x) = Right x

instance ToMultipart Tmp SendMessageRequest where
  toMultipart form =
    MultipartData
      ( [ Input "type" $ show form._type,
          Input "messageId" form.messageId
        ]
          <> [Input "scheduledTime" $ (TE.decodeUtf8 . BLC.toStrict . A.encode) form.scheduledTime | isMaybe form.scheduledTime]
      )
      (maybe [] (\file -> [FileData "csvFile" (T.pack file) "text/csv" file]) form.csvFile)
    where
      isMaybe :: Maybe a -> Bool
      isMaybe Nothing = False
      isMaybe (Just _) = True
