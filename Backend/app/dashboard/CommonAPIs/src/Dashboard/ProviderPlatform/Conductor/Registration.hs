{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Conductor.Registration where

import API.Types.ProviderPlatform.Conductor.Endpoints.Registration
import Dashboard.Common as ReExport
import Kernel.Prelude

instance HideSecrets ConductorRegisterReq where
  type ReqWithoutSecrets ConductorRegisterReq = ConductorRegisterTReq
  hideSecrets ConductorRegisterReq {..} = ConductorRegisterTReq {..}

instance HideSecrets ConductorBulkRegisterReq where
  type ReqWithoutSecrets ConductorBulkRegisterReq = ConductorBulkRegisterTReq
  hideSecrets ConductorBulkRegisterReq {..} =
    ConductorBulkRegisterTReq {conductors = map hideSecrets conductors}
