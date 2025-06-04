{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Fleet.RegistrationV2 (module ReExport) where

import API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import Dashboard.Common as ReExport

instance HideSecrets FleetOwnerRegisterReqV2 where
  type ReqWithoutSecrets FleetOwnerRegisterReqV2 = FleetOwnerRegisterTReqV2
  hideSecrets FleetOwnerRegisterReqV2 {..} = FleetOwnerRegisterTReqV2 {..}
