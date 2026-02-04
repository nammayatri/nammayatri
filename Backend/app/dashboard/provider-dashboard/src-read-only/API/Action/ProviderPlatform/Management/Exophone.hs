{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Exophone
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Exophone
import qualified Domain.Action.ProviderPlatform.Management.Exophone
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

type API = ("exophone" :> (PostExophoneCreate :<|> GetExophoneList :<|> GetExophone :<|> PostExophoneUpdate :<|> DeleteExophoneDelete))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postExophoneCreate merchantId city :<|> getExophoneList merchantId city :<|> getExophone merchantId city :<|> postExophoneUpdate merchantId city :<|> deleteExophoneDelete merchantId city

type PostExophoneCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.EXOPHONE / 'API.Types.ProviderPlatform.Management.Exophone.POST_EXOPHONE_CREATE)
      :> API.Types.ProviderPlatform.Management.Exophone.PostExophoneCreate
  )

type GetExophoneList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.EXOPHONE / 'API.Types.ProviderPlatform.Management.Exophone.GET_EXOPHONE_LIST)
      :> API.Types.ProviderPlatform.Management.Exophone.GetExophoneList
  )

type GetExophone =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.EXOPHONE / 'API.Types.ProviderPlatform.Management.Exophone.GET_EXOPHONE)
      :> API.Types.ProviderPlatform.Management.Exophone.GetExophone
  )

type PostExophoneUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.EXOPHONE / 'API.Types.ProviderPlatform.Management.Exophone.POST_EXOPHONE_UPDATE)
      :> API.Types.ProviderPlatform.Management.Exophone.PostExophoneUpdate
  )

type DeleteExophoneDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.EXOPHONE / 'API.Types.ProviderPlatform.Management.Exophone.DELETE_EXOPHONE_DELETE)
      :> API.Types.ProviderPlatform.Management.Exophone.DeleteExophoneDelete
  )

postExophoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Exophone.CreateExophoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Exophone.ExophoneRes)
postExophoneCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Exophone.postExophoneCreate merchantShortId opCity apiTokenInfo req

getExophoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Exophone.ExophoneListRes)
getExophoneList merchantShortId opCity apiTokenInfo merchantOperatingCityId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Exophone.getExophoneList merchantShortId opCity apiTokenInfo merchantOperatingCityId

getExophone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Exophone.ExophoneRes)
getExophone merchantShortId opCity apiTokenInfo exophoneId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Exophone.getExophone merchantShortId opCity apiTokenInfo exophoneId

postExophoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Exophone.UpdateExophoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postExophoneUpdate merchantShortId opCity apiTokenInfo exophoneId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Exophone.postExophoneUpdate merchantShortId opCity apiTokenInfo exophoneId req

deleteExophoneDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteExophoneDelete merchantShortId opCity apiTokenInfo exophoneId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Exophone.deleteExophoneDelete merchantShortId opCity apiTokenInfo exophoneId
