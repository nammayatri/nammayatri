{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Dashboard.ProviderPlatform.Management.RSFReconciliation (module ReExport) where

import API.Types.ProviderPlatform.Management.Endpoints.RSFReconciliation
import Dashboard.Common as ReExport
import Dashboard.Common.RSFReconciliation
import Kernel.Prelude
import Kernel.Types.HideSecrets

instance HideSecrets BankVerifyReq where
  hideSecrets = identity

instance HideSecrets ManualConfirmReq where
  hideSecrets = identity
