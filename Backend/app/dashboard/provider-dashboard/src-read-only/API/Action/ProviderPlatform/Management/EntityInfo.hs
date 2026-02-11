{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.EntityInfo
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.EntityInfo
import qualified Domain.Action.ProviderPlatform.Management.EntityInfo
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

type API = ("entityInfo" :> (GetEntityInfoList :<|> PostEntityInfoUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getEntityInfoList merchantId city :<|> postEntityInfoUpdate merchantId city

type GetEntityInfoList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.ENTITY_INFO) / ('API.Types.ProviderPlatform.Management.EntityInfo.GET_ENTITY_INFO_LIST))
      :> API.Types.ProviderPlatform.Management.EntityInfo.GetEntityInfoList
  )

type PostEntityInfoUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.ENTITY_INFO) / ('API.Types.ProviderPlatform.Management.EntityInfo.POST_ENTITY_INFO_UPDATE))
      :> API.Types.ProviderPlatform.Management.EntityInfo.PostEntityInfoUpdate
  )

getEntityInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.EntityInfo.EntityExtraInformation)
getEntityInfoList merchantShortId opCity apiTokenInfo entityType entityId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.EntityInfo.getEntityInfoList merchantShortId opCity apiTokenInfo entityType entityId

postEntityInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.EntityInfo.UpdateEntityInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEntityInfoUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.EntityInfo.postEntityInfoUpdate merchantShortId opCity apiTokenInfo req
