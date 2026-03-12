{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Dashboard.ProviderPlatform.Management.KnowledgeCenter (module ReExport) where

import API.Types.ProviderPlatform.Management.Endpoints.KnowledgeCenter
import Dashboard.Common as ReExport
import Dashboard.Common.KnowledgeCenter
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel
import Kernel.Types.HideSecrets

instance HideSecrets KnowledgeCenterUploadImageReq where
  hideSecrets = Kernel.identity

instance HideSecrets KnowledgeCenterUploadImageResp where
  hideSecrets = Kernel.identity

instance HideSecrets KnowledgeCenterUploadVideoLinkReq where
  hideSecrets = Kernel.identity

instance HideSecrets KnowledgeCenterUploadVideoLinkResp where
  hideSecrets = Kernel.identity
