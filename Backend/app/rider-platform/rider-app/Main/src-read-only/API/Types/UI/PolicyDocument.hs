{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PolicyDocument where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.PolicyAndComplianceDocument
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data PolicyAcceptReq = PolicyAcceptReq {policyDocId :: Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyDocumentResp = PolicyDocumentResp
  { createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument,
    isMandatory :: Kernel.Prelude.Bool,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyType :: Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyDocumentWithAcceptance = PolicyDocumentWithAcceptance
  { accepted :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument,
    isMandatory :: Kernel.Prelude.Bool,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    policyType :: Kernel.Prelude.Text,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyLatestResp = PolicyLatestResp {documents :: [PolicyDocumentResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyListResp = PolicyListResp {groups :: [PolicyTypeGroup]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PolicyTypeGroup = PolicyTypeGroup {entries :: [PolicyDocumentWithAcceptance], policyType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
