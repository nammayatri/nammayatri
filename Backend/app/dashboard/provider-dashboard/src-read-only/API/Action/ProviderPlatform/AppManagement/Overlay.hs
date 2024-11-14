{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.Overlay
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Overlay
import qualified Domain.Action.ProviderPlatform.AppManagement.Overlay
import qualified "lib-dashboard" Domain.Types.Merchant
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

type API = ("overlay" :> (PostOverlayCreate :<|> PostOverlayDelete :<|> GetOverlayList :<|> GetOverlayInfo :<|> PostOverlaySchedule))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOverlayCreate merchantId city :<|> postOverlayDelete merchantId city :<|> getOverlayList merchantId city :<|> getOverlayInfo merchantId city :<|> postOverlaySchedule merchantId city

type PostOverlayCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.OVERLAY / 'API.Types.Dashboard.AppManagement.Overlay.POST_OVERLAY_CREATE)
      :> API.Types.Dashboard.AppManagement.Overlay.PostOverlayCreate
  )

type PostOverlayDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.OVERLAY / 'API.Types.Dashboard.AppManagement.Overlay.POST_OVERLAY_DELETE)
      :> API.Types.Dashboard.AppManagement.Overlay.PostOverlayDelete
  )

type GetOverlayList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.OVERLAY / 'API.Types.Dashboard.AppManagement.Overlay.GET_OVERLAY_LIST)
      :> API.Types.Dashboard.AppManagement.Overlay.GetOverlayList
  )

type GetOverlayInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.OVERLAY / 'API.Types.Dashboard.AppManagement.Overlay.GET_OVERLAY_INFO)
      :> API.Types.Dashboard.AppManagement.Overlay.GetOverlayInfo
  )

type PostOverlaySchedule =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.OVERLAY / 'API.Types.Dashboard.AppManagement.Overlay.POST_OVERLAY_SCHEDULE)
      :> API.Types.Dashboard.AppManagement.Overlay.PostOverlaySchedule
  )

postOverlayCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Overlay.CreateOverlayReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlayCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Overlay.postOverlayCreate merchantShortId opCity apiTokenInfo req

postOverlayDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Overlay.DeleteOverlayReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlayDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Overlay.postOverlayDelete merchantShortId opCity apiTokenInfo req

getOverlayList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Overlay.ListOverlayResp)
getOverlayList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Overlay.getOverlayList merchantShortId opCity apiTokenInfo

getOverlayInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Overlay.OverlayInfoResp)
getOverlayInfo merchantShortId opCity apiTokenInfo udf1 overlayKey = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Overlay.getOverlayInfo merchantShortId opCity apiTokenInfo udf1 overlayKey

postOverlaySchedule :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Overlay.ScheduleOverlay -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOverlaySchedule merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Overlay.postOverlaySchedule merchantShortId opCity apiTokenInfo req
