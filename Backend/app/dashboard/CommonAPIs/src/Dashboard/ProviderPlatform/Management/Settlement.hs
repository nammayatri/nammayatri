{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Settlement where

import API.Types.ProviderPlatform.Management.Endpoints.Settlement
import Dashboard.Common as ReExport
import qualified Kernel.Prelude

instance HideSecrets ChargebackRespondReq where
  hideSecrets = Kernel.Prelude.identity
