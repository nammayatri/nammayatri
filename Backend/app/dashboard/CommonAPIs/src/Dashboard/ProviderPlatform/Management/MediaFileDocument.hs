{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.MediaFileDocument where

import API.Types.ProviderPlatform.Management.Endpoints.MediaFileDocument
import Dashboard.Common as ReExport

instance HideSecrets UploadMediaFileDocumentReq where
  type ReqWithoutSecrets UploadMediaFileDocumentReq = UploadMediaFileDocumentTReq
  hideSecrets UploadMediaFileDocumentReq {..} = UploadMediaFileDocumentTReq {..}

instance HideSecrets MediaFileDocumentResp where
  type ReqWithoutSecrets MediaFileDocumentResp = MediaFileDocumentTResp
  hideSecrets MediaFileDocumentResp {..} = MediaFileDocumentTResp {..}
