{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Dashboard.RiderPlatform.Management.MultiModal (module ReExport) where

import API.Types.RiderPlatform.Management.Endpoints.MultiModal
import Dashboard.Common as ReExport
import qualified Data.Text as T hiding (length)
import Kernel.Prelude
import Kernel.ServantMultipart

instance FromMultipart Tmp PreprocessFRFSDataReq where
  fromMultipart form = do
    PreprocessFRFSDataReq
      <$> fmap (read . T.unpack) (lookupInput "vehicleType" form)
      <*> fmap (read . T.unpack) (lookupInput "fileFormat" form)
      <*> fmap fdPayload (lookupFile "file" form)
      <*> fmap (read . T.unpack) (lookupInput "inputDataType" form)

instance ToMultipart Tmp PreprocessFRFSDataReq where
  toMultipart form =
    MultipartData
      [ Input "vehicleType" (T.pack $ show form.vehicleType),
        Input "fileFormat" (T.pack $ show form.fileFormat),
        Input "inputDataType" (T.pack $ show form.inputDataType)
      ]
      [FileData "file" (T.pack form.file) "" (form.file)]
