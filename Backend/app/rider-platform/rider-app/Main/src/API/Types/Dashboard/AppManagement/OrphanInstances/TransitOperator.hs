{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types.Dashboard.AppManagement.OrphanInstances.TransitOperator (module ReExport) where

import API.Types.Dashboard.AppManagement.Endpoints.TransitOperator
import Dashboard.Common as ReExport
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart

instance FromMultipart Tmp UpsertDeviceVehicleMappingReq where
  fromMultipart form = do
    UpsertDeviceVehicleMappingReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpsertDeviceVehicleMappingReq where
  toMultipart form =
    MultipartData
      []
      [ FileData
          "file"
          (T.pack form.file) -- filename
          "" -- content-type
          form.file -- payload: FilePath
      ]
