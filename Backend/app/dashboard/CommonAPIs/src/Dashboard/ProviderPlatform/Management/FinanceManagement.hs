{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.FinanceManagement where

import API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement
import Dashboard.Common as ReExport
import qualified Kernel.Prelude

instance HideSecrets ReconciliationTriggerReq where
  hideSecrets = Kernel.Prelude.identity

instance HideSecrets TdsReimbursementRequestSubmitReq where
  type ReqWithoutSecrets TdsReimbursementRequestSubmitReq = TdsReimbursementRequestSubmitTReq
  hideSecrets TdsReimbursementRequestSubmitReq {..} = TdsReimbursementRequestSubmitTReq {..}

instance HideSecrets TdsReimbursementRejectReq where
  hideSecrets = Kernel.Prelude.identity
