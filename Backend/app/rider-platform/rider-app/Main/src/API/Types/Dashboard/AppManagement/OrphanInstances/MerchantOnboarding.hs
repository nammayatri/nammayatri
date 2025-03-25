{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module API.Types.Dashboard.AppManagement.OrphanInstances.MerchantOnboarding (module ReExport) where

import API.Types.Dashboard.AppManagement.Endpoints.MerchantOnboarding
import Dashboard.Common as ReExport
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
