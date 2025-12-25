{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types.Dashboard.AppManagement.OrphanInstances.MerchantOnboarding (module ReExport) where

import API.Types.Dashboard.AppManagement.Endpoints.MerchantOnboarding
import Dashboard.Common as ReExport
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart

instance FromMultipart Tmp UploadFileRequest where
  fromMultipart form = do
    UploadFileRequest
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)
      <*> fmap (read . T.unpack) (lookupInput "fileType" form)

instance ToMultipart Tmp UploadFileRequest where
  toMultipart uploadFileRequest =
    MultipartData
      [Input "fileType" (show uploadFileRequest.fileType)]
      [FileData "file" (T.pack uploadFileRequest.file) "" (uploadFileRequest.file)]
