{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Operator.FleetManagement (module ReExport) where

import API.Types.ProviderPlatform.Operator.Endpoints.FleetManagement
import Dashboard.Common as ReExport

instance HideSecrets FleetOwnerRegisterReq where
  type ReqWithoutSecrets FleetOwnerRegisterReq = FleetOwnerRegisterTReq
  hideSecrets FleetOwnerRegisterReq {..} = FleetOwnerRegisterTReq {..}
