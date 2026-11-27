{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Permit.Endpoints.Permit where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Permit.Api.Types
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Domain.Types.PermitApplicationDocument
import qualified Lib.Permit.Types
import Servant hiding (Summary)
import Servant.Client

type API = ("permit" :> (GetPermitApplicationList :<|> GetPermitApplication :<|> GetPermitApplicationDocumentHelper :<|> PostPermitApplicationDecisionHelper :<|> PostPermitApplicationDcDecisionHelper :<|> PostPermitPassVerifyHelper))

type GetPermitApplicationList =
  ( "application" :> "list" :> QueryParam "status" Lib.Permit.Domain.Types.PermitApplication.PermitStatus :> QueryParam "permitDate" Data.Time.Day
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "permitType"
           Lib.Permit.Types.PermitType
      :> Get
           ('[JSON])
           Lib.Permit.Api.Types.PermitApplicationListRes
  )

type GetPermitApplication =
  ( "application" :> Capture "applicationId" (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> Get
           ('[JSON])
           Lib.Permit.Api.Types.PermitApplicationDetailsRes
  )

type GetPermitApplicationDocument =
  ( "application" :> Capture "applicationId" (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication) :> "document"
      :> Capture
           "documentType"
           Lib.Permit.Domain.Types.PermitApplicationDocument.PermitDocumentType
      :> Get
           ('[JSON])
           Lib.Permit.Api.Types.PermitDocumentRes
  )

type GetPermitApplicationDocumentHelper =
  ( "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "document"
      :> Capture
           "documentType"
           Lib.Permit.Domain.Types.PermitApplicationDocument.PermitDocumentType
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "requestorName"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           Lib.Permit.Api.Types.PermitDocumentRes
  )

type PostPermitApplicationDecision =
  ( "application" :> Capture "applicationId" (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication) :> "decision"
      :> ReqBody
           ('[JSON])
           Lib.Permit.Api.Types.PermitDecisionReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostPermitApplicationDecisionHelper =
  ( "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "decision"
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "requestorName"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           Lib.Permit.Api.Types.PermitDecisionReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostPermitApplicationDcDecision =
  ( "application" :> Capture "applicationId" (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication) :> "dcDecision"
      :> ReqBody
           ('[JSON])
           Lib.Permit.Api.Types.PermitDecisionReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostPermitApplicationDcDecisionHelper =
  ( "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "dcDecision"
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "requestorName"
           Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           Lib.Permit.Api.Types.PermitDecisionReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostPermitPassVerify = ("pass" :> "verify" :> ReqBody ('[JSON]) Lib.Permit.Api.Types.PermitVerifyPassReq :> Post ('[JSON]) Lib.Permit.Api.Types.PermitVerifyPassRes)

type PostPermitPassVerifyHelper =
  ( "pass" :> "verify" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorName" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           Lib.Permit.Api.Types.PermitVerifyPassReq
      :> Post ('[JSON]) Lib.Permit.Api.Types.PermitVerifyPassRes
  )

data PermitAPIs = PermitAPIs
  { getPermitApplicationList :: (Kernel.Prelude.Maybe (Lib.Permit.Domain.Types.PermitApplication.PermitStatus) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Permit.Types.PermitType -> EulerHS.Types.EulerClient Lib.Permit.Api.Types.PermitApplicationListRes),
    getPermitApplication :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> EulerHS.Types.EulerClient Lib.Permit.Api.Types.PermitApplicationDetailsRes),
    getPermitApplicationDocument :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Lib.Permit.Domain.Types.PermitApplicationDocument.PermitDocumentType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Permit.Api.Types.PermitDocumentRes),
    postPermitApplicationDecision :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitDecisionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postPermitApplicationDcDecision :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitDecisionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postPermitPassVerify :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitVerifyPassReq -> EulerHS.Types.EulerClient Lib.Permit.Api.Types.PermitVerifyPassRes)
  }

mkPermitAPIs :: (Client EulerHS.Types.EulerClient API -> PermitAPIs)
mkPermitAPIs permitClient = (PermitAPIs {..})
  where
    getPermitApplicationList :<|> getPermitApplication :<|> getPermitApplicationDocument :<|> postPermitApplicationDecision :<|> postPermitApplicationDcDecision :<|> postPermitPassVerify = permitClient

data PermitUserActionType
  = GET_PERMIT_APPLICATION_LIST
  | GET_PERMIT_APPLICATION
  | GET_PERMIT_APPLICATION_DOCUMENT
  | POST_PERMIT_APPLICATION_DECISION
  | POST_PERMIT_APPLICATION_DC_DECISION
  | POST_PERMIT_PASS_VERIFY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''PermitUserActionType)])
