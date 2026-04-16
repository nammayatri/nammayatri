{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.AdminRequest
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.AdminRequest
import qualified Domain.Action.ProviderPlatform.AppManagement.AdminRequest
import qualified "dynamic-offer-driver-app" Domain.Types.AdminRequest
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("adminRequest" :> (PostAdminRequestCreate :<|> GetAdminRequestList :<|> PostAdminRequestRespond))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postAdminRequestCreate merchantId city :<|> getAdminRequestList merchantId city :<|> postAdminRequestRespond merchantId city

type PostAdminRequestCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.ADMIN_REQUEST / 'API.Types.Dashboard.AppManagement.AdminRequest.POST_ADMIN_REQUEST_CREATE)
      :> API.Types.Dashboard.AppManagement.AdminRequest.PostAdminRequestCreate
  )

type GetAdminRequestList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.ADMIN_REQUEST / 'API.Types.Dashboard.AppManagement.AdminRequest.GET_ADMIN_REQUEST_LIST)
      :> API.Types.Dashboard.AppManagement.AdminRequest.GetAdminRequestList
  )

type PostAdminRequestRespond =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.ADMIN_REQUEST / 'API.Types.Dashboard.AppManagement.AdminRequest.POST_ADMIN_REQUEST_RESPOND)
      :> API.Types.Dashboard.AppManagement.AdminRequest.PostAdminRequestRespond
  )

postAdminRequestCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.AdminRequest.CreateAdminRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAdminRequestCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.AdminRequest.postAdminRequestCreate merchantShortId opCity apiTokenInfo req

getAdminRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest) -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdminRequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.ActionType -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.ReferenceTable -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.Dashboard.AppManagement.AdminRequest.AdminRequestResp)
getAdminRequestList merchantShortId opCity apiTokenInfo limit offset adminRequestId status personId excludeCurrentAdminMaker actionType adjustmentType referenceType referenceId referenceTable source adminMakerId adminCheckerId from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.AdminRequest.getAdminRequestList merchantShortId opCity apiTokenInfo limit offset adminRequestId status personId excludeCurrentAdminMaker actionType adjustmentType referenceType referenceId referenceTable source adminMakerId adminCheckerId from to

postAdminRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest -> API.Types.Dashboard.AppManagement.AdminRequest.RespondAdminRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAdminRequestRespond merchantShortId opCity apiTokenInfo adminRequestId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.AdminRequest.postAdminRequestRespond merchantShortId opCity apiTokenInfo adminRequestId req
