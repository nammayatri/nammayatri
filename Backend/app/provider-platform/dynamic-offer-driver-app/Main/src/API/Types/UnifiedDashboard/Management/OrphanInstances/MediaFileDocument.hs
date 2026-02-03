{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types.UnifiedDashboard.Management.OrphanInstances.MediaFileDocument (module ReExport) where

import API.Types.UnifiedDashboard.Common as ReExport
import API.Types.UnifiedDashboard.Management.Endpoints.MediaFileDocument

instance HideSecrets UploadMediaFileDocumentReq where
  type ReqWithoutSecrets UploadMediaFileDocumentReq = UploadMediaFileDocumentTReq
  hideSecrets UploadMediaFileDocumentReq {..} = UploadMediaFileDocumentTReq {..}

instance HideSecrets MediaFileDocumentResp where
  type ReqWithoutSecrets MediaFileDocumentResp = MediaFileDocumentTResp
  hideSecrets MediaFileDocumentResp {..} = MediaFileDocumentTResp {..}
