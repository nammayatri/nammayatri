module IssueManagement.Common.Beckn.Issue where

-- import qualified IssueManagement.Beckn.ACL.Issue as ACL
-- import qualified Beckn.ACL.IGM.OnIssue as ACL
import qualified BecknV2.IGM.APIs as Spec
-- import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
-- import Kernel.Types.Common
-- import Environment
-- import qualified IGM.Types as Spec
-- import qualified IGM.Utils as Utils
-- import Kernel.Prelude
-- import qualified Kernel.Storage.Hedis as Redis

-- import Kernel.Types.Error

-- import Kernel.Utils.Common

import IssueManagement.Common
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

-- import qualified IssueManagement.SharedLogic.CallIGMAPI as CallBAP

type IssueAPI =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Spec.IssueAPI
