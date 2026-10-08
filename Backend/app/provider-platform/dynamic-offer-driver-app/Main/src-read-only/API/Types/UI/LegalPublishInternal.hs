{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.LegalPublishInternal where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Domain.Types.PolicyAndComplianceDocument
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data LegalPublishReq = LegalPublishReq
  { effectiveDate :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    entityType :: Dashboard.Common.LegalEntityType,
    isMandatory :: Kernel.Prelude.Bool,
    merchantShortId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatingCity :: Kernel.Prelude.Text,
    policyType :: Dashboard.Common.PolicyType,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegalPublishResp = LegalPublishResp {created :: Kernel.Prelude.Bool, id :: Kernel.Types.Id.Id Domain.Types.PolicyAndComplianceDocument.PolicyAndComplianceDocument}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
