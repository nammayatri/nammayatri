{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.KnowledgeCenter
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.KnowledgeCenter
import qualified Domain.Action.ProviderPlatform.Management.KnowledgeCenter
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

type API = ("knowledgeCenter" :> (GetKnowledgeCenterGetDocument :<|> GetKnowledgeCenterSopList :<|> PostKnowledgeCenterSopUpload :<|> PostKnowledgeCenterVideoUploadLink :<|> PostKnowledgeCenterVideoConfirm :<|> PutKnowledgeCenterSopTypeRename :<|> DeleteKnowledgeCenterSopDocument :<|> DeleteKnowledgeCenterSopType))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getKnowledgeCenterGetDocument merchantId city :<|> getKnowledgeCenterSopList merchantId city :<|> postKnowledgeCenterSopUpload merchantId city :<|> postKnowledgeCenterVideoUploadLink merchantId city :<|> postKnowledgeCenterVideoConfirm merchantId city :<|> putKnowledgeCenterSopTypeRename merchantId city :<|> deleteKnowledgeCenterSopDocument merchantId city :<|> deleteKnowledgeCenterSopType merchantId city

type GetKnowledgeCenterGetDocument =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.GET_KNOWLEDGE_CENTER_GET_DOCUMENT))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterGetDocument
  )

type GetKnowledgeCenterSopList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.GET_KNOWLEDGE_CENTER_SOP_LIST))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterSopList
  )

type PostKnowledgeCenterSopUpload =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.POST_KNOWLEDGE_CENTER_SOP_UPLOAD))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.PostKnowledgeCenterSopUpload
  )

type PostKnowledgeCenterVideoUploadLink =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.POST_KNOWLEDGE_CENTER_VIDEO_UPLOAD_LINK))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.PostKnowledgeCenterVideoUploadLink
  )

type PostKnowledgeCenterVideoConfirm =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.POST_KNOWLEDGE_CENTER_VIDEO_CONFIRM))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.PostKnowledgeCenterVideoConfirm
  )

type PutKnowledgeCenterSopTypeRename =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.PUT_KNOWLEDGE_CENTER_SOP_TYPE_RENAME))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.PutKnowledgeCenterSopTypeRename
  )

type DeleteKnowledgeCenterSopDocument =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.DELETE_KNOWLEDGE_CENTER_SOP_DOCUMENT))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.DeleteKnowledgeCenterSopDocument
  )

type DeleteKnowledgeCenterSopType =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.KNOWLEDGE_CENTER) / ('API.Types.ProviderPlatform.Management.KnowledgeCenter.DELETE_KNOWLEDGE_CENTER_SOP_TYPE))
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.DeleteKnowledgeCenterSopType
  )

getKnowledgeCenterGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterDocumentResp)
getKnowledgeCenterGetDocument merchantShortId opCity apiTokenInfo knowledgeCenterId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.getKnowledgeCenterGetDocument merchantShortId opCity apiTokenInfo knowledgeCenterId

getKnowledgeCenterSopList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterSopListResp)
getKnowledgeCenterSopList merchantShortId opCity apiTokenInfo merchantOperatingCityId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.getKnowledgeCenterSopList merchantShortId opCity apiTokenInfo merchantOperatingCityId

postKnowledgeCenterSopUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageResp)
postKnowledgeCenterSopUpload merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.postKnowledgeCenterSopUpload merchantShortId opCity apiTokenInfo req

postKnowledgeCenterVideoUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadVideoLinkReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadVideoLinkResp)
postKnowledgeCenterVideoUploadLink merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.postKnowledgeCenterVideoUploadLink merchantShortId opCity apiTokenInfo req

postKnowledgeCenterVideoConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterConfirmVideoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postKnowledgeCenterVideoConfirm merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.postKnowledgeCenterVideoConfirm merchantShortId opCity apiTokenInfo req

putKnowledgeCenterSopTypeRename :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterRenameSopTypeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putKnowledgeCenterSopTypeRename merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.putKnowledgeCenterSopTypeRename merchantShortId opCity apiTokenInfo req

deleteKnowledgeCenterSopDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopDocument merchantShortId opCity apiTokenInfo knowledgeCenterId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.deleteKnowledgeCenterSopDocument merchantShortId opCity apiTokenInfo knowledgeCenterId

deleteKnowledgeCenterSopType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopType merchantShortId opCity apiTokenInfo sopType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.KnowledgeCenter.deleteKnowledgeCenterSopType merchantShortId opCity apiTokenInfo sopType
