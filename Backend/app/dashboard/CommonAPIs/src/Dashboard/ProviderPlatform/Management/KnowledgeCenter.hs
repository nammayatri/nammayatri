{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Dashboard.ProviderPlatform.Management.KnowledgeCenter (module ReExport) where

import API.Types.ProviderPlatform.Management.Endpoints.KnowledgeCenter
import Dashboard.Common as ReExport
import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel
import Kernel.Types.HideSecrets

data KnowledgeCenterUploadImageTReq = KnowledgeCenterUploadImageTReq
  { sopType :: Kernel.Prelude.Text,
    documentName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileExtension :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON)

instance HideSecrets KnowledgeCenterUploadImageReq where
  type ReqWithoutSecrets KnowledgeCenterUploadImageReq = KnowledgeCenterUploadImageTReq
  hideSecrets KnowledgeCenterUploadImageReq {sopType, documentName, fileExtension} =
    KnowledgeCenterUploadImageTReq {sopType = sopType, documentName = documentName, fileExtension = fileExtension}

instance HideSecrets KnowledgeCenterUploadImageResp where
  hideSecrets = Kernel.identity

instance HideSecrets KnowledgeCenterRenameSopTypeReq where
  hideSecrets = Kernel.identity

instance HideSecrets KnowledgeCenterDocumentItem where
  hideSecrets = Kernel.identity
