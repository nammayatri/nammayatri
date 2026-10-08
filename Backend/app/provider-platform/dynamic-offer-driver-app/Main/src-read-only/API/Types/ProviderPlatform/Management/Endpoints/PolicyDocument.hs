{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.PolicyDocument where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data PolicyCreateReq = PolicyCreateReq
  { policyType :: Dashboard.Common.PolicyType,
    entityType :: Kernel.Prelude.Maybe Dashboard.Common.LegalEntityType,
    version :: Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    effectiveDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    isMandatory :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PolicyCreateReq where
  hideSecrets = Kernel.Prelude.identity

data PolicyCreateResp = PolicyCreateResp {id :: Kernel.Prelude.Text, policyType :: Dashboard.Common.PolicyType, entityType :: Kernel.Prelude.Maybe Dashboard.Common.LegalEntityType, version :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyDocumentMgmtResp = PolicyDocumentMgmtResp
  { id :: Kernel.Prelude.Text,
    policyType :: Dashboard.Common.PolicyType,
    entityType :: Kernel.Prelude.Maybe Dashboard.Common.LegalEntityType,
    version :: Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    effectiveDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    isMandatory :: Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyListMgmtResp = PolicyListMgmtResp {documents :: [PolicyDocumentMgmtResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyUpdateReq = PolicyUpdateReq
  { url :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    effectiveDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    isMandatory :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PolicyUpdateReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("policyDocument" :> (PostPolicyDocumentCreate :<|> PostPolicyDocumentUpdate :<|> GetPolicyDocumentList))

type PostPolicyDocumentCreate = ("create" :> ReqBody '[JSON] PolicyCreateReq :> Post '[JSON] PolicyCreateResp)

type PostPolicyDocumentUpdate =
  ( Capture "policyDocId" (Kernel.Types.Id.Id Dashboard.Common.PolicyAndComplianceDocument) :> "update" :> ReqBody '[JSON] PolicyUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetPolicyDocumentList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] PolicyListMgmtResp)

data PolicyDocumentAPIs = PolicyDocumentAPIs
  { postPolicyDocumentCreate :: PolicyCreateReq -> EulerHS.Types.EulerClient PolicyCreateResp,
    postPolicyDocumentUpdate :: Kernel.Types.Id.Id Dashboard.Common.PolicyAndComplianceDocument -> PolicyUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPolicyDocumentList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient PolicyListMgmtResp
  }

mkPolicyDocumentAPIs :: (Client EulerHS.Types.EulerClient API -> PolicyDocumentAPIs)
mkPolicyDocumentAPIs policyDocumentClient = (PolicyDocumentAPIs {..})
  where
    postPolicyDocumentCreate :<|> postPolicyDocumentUpdate :<|> getPolicyDocumentList = policyDocumentClient

data PolicyDocumentUserActionType
  = POST_POLICY_DOCUMENT_CREATE
  | POST_POLICY_DOCUMENT_UPDATE
  | GET_POLICY_DOCUMENT_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PolicyDocumentUserActionType])
