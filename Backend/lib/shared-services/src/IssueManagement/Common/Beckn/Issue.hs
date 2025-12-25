module IssueManagement.Common.Beckn.Issue where

import qualified BecknV2.IGM.APIs as Spec
import IssueManagement.Common
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (Unauthorized, throwError)

type OnDemandAPI =
  OnDemandBAPFlowAPI
    :<|> OnDemandBPPFlowAPI

type OnDemandBAPFlowAPI =
  "beckn" :> "cab" :> "v1"
    :> Capture "merchantId" (Id Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ( Spec.OnIssueAPI
           :<|> Spec.OnIssueStatusAPI
       )

type OnDemandBPPFlowAPI =
  "beckn"
    :> Capture "merchantId" (Id Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ( Spec.IssueAPI
           :<|> Spec.IssueStatusAPI
       )

type PublicTransportAPI =
  "beckn" :> "frfs" :> "v1"
    :> Capture "merchantId" (Id Merchant)
    :> SignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> ( Spec.OnIssueAPI
           :<|> Spec.OnIssueStatusAPI
       )

type OnIssueAPI = Spec.OnIssueAPI

type IssueStatusAPI = Spec.IssueStatusAPI

type OnIssueStatusAPI = Spec.OnIssueStatusAPI
