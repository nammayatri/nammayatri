{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.MultiModal
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.MultiModal
import qualified Domain.Action.RiderPlatform.Management.MultiModal
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("multiModal" :> (PostMultiModalMultimodalFrfsDataPreprocess :<|> PostMultiModalMultimodalFrfsDataStatus :<|> PostMultiModalMultimodalFrfsDataVersionIsReady :<|> PostMultiModalMultimodalFrfsDataVersionApply))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMultiModalMultimodalFrfsDataPreprocess merchantId city :<|> postMultiModalMultimodalFrfsDataStatus merchantId city :<|> postMultiModalMultimodalFrfsDataVersionIsReady merchantId city :<|> postMultiModalMultimodalFrfsDataVersionApply merchantId city

type PostMultiModalMultimodalFrfsDataPreprocess =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MULTI_MODAL / 'API.Types.RiderPlatform.Management.MultiModal.POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_PREPROCESS)
      :> API.Types.RiderPlatform.Management.MultiModal.PostMultiModalMultimodalFrfsDataPreprocess
  )

type PostMultiModalMultimodalFrfsDataStatus =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MULTI_MODAL / 'API.Types.RiderPlatform.Management.MultiModal.POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_STATUS)
      :> API.Types.RiderPlatform.Management.MultiModal.PostMultiModalMultimodalFrfsDataStatus
  )

type PostMultiModalMultimodalFrfsDataVersionIsReady =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MULTI_MODAL / 'API.Types.RiderPlatform.Management.MultiModal.POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_VERSION_IS_READY)
      :> API.Types.RiderPlatform.Management.MultiModal.PostMultiModalMultimodalFrfsDataVersionIsReady
  )

type PostMultiModalMultimodalFrfsDataVersionApply =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.MULTI_MODAL / 'API.Types.RiderPlatform.Management.MultiModal.POST_MULTI_MODAL_MULTIMODAL_FRFS_DATA_VERSION_APPLY)
      :> API.Types.RiderPlatform.Management.MultiModal.PostMultiModalMultimodalFrfsDataVersionApply
  )

postMultiModalMultimodalFrfsDataPreprocess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp)
postMultiModalMultimodalFrfsDataPreprocess merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.MultiModal.postMultiModalMultimodalFrfsDataPreprocess merchantShortId opCity apiTokenInfo req

postMultiModalMultimodalFrfsDataStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatus -> Environment.FlowHandler API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusResp)
postMultiModalMultimodalFrfsDataStatus merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.MultiModal.postMultiModalMultimodalFrfsDataStatus merchantShortId opCity apiTokenInfo req

postMultiModalMultimodalFrfsDataVersionIsReady :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.MultiModal.ReadyVersionReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp)
postMultiModalMultimodalFrfsDataVersionIsReady merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.MultiModal.postMultiModalMultimodalFrfsDataVersionIsReady merchantShortId opCity apiTokenInfo req

postMultiModalMultimodalFrfsDataVersionApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.MultiModal.ApplyVersionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMultiModalMultimodalFrfsDataVersionApply merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.MultiModal.postMultiModalMultimodalFrfsDataVersionApply merchantShortId opCity apiTokenInfo req
