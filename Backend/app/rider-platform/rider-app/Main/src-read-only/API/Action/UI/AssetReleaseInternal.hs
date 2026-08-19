{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AssetReleaseInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AssetReleaseInternal
import qualified Domain.Action.UI.AssetReleaseInternal
import qualified Domain.Types.AssetRelease
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "assetRelease" :> "publish" :> Header "token" Kernel.Prelude.Text :> ReqBody ('[JSON]) API.Types.UI.AssetReleaseInternal.AssetPublishReq
      :> Post
           ('[JSON])
           API.Types.UI.AssetReleaseInternal.AssetPublishResp
      :<|> "assetRelease"
      :> "rollback"
      :> Header "token" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.AssetReleaseInternal.AssetRollbackReq
      :> Post
           ('[JSON])
           API.Types.UI.AssetReleaseInternal.AssetRollbackResp
      :<|> "assetRelease"
      :> QueryParam
           "assetType"
           Domain.Types.AssetRelease.AssetType
      :> QueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> QueryParam
           "merchantShortId"
           Kernel.Prelude.Text
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           ((Kernel.Prelude.Maybe API.Types.UI.AssetReleaseInternal.AssetReleaseResp))
  )

handler :: Environment.FlowServer API
handler = postAssetReleasePublish :<|> postAssetReleaseRollback :<|> getAssetRelease

postAssetReleasePublish :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.AssetReleaseInternal.AssetPublishReq -> Environment.FlowHandler API.Types.UI.AssetReleaseInternal.AssetPublishResp)
postAssetReleasePublish a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.AssetReleaseInternal.postAssetReleasePublish a2 a1

postAssetReleaseRollback :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.AssetReleaseInternal.AssetRollbackReq -> Environment.FlowHandler API.Types.UI.AssetReleaseInternal.AssetRollbackResp)
postAssetReleaseRollback a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.AssetReleaseInternal.postAssetReleaseRollback a2 a1

getAssetRelease :: (Kernel.Prelude.Maybe (Domain.Types.AssetRelease.AssetType) -> Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler (Kernel.Prelude.Maybe API.Types.UI.AssetReleaseInternal.AssetReleaseResp))
getAssetRelease a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.AssetReleaseInternal.getAssetRelease a4 a3 a2 a1
