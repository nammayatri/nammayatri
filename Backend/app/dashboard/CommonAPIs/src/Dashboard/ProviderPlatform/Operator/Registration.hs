{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Operator.Registration (module ReExport) where

import API.Types.ProviderPlatform.Operator.Endpoints.Registration
import Dashboard.Common as ReExport

instance HideSecrets OperatorRegisterReq where
  type ReqWithoutSecrets OperatorRegisterReq = OperatorRegisterTReq
  hideSecrets OperatorRegisterReq {..} = OperatorRegisterTReq {..}
