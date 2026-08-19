{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AssetManifest
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AssetManifest
import qualified Control.Lens
import qualified Domain.Action.UI.AssetManifest
import qualified Domain.Types.AssetRelease
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "assetManifest" :> QueryParam "assetTypes" [Domain.Types.AssetRelease.AssetType] :> Get '[JSON] API.Types.UI.AssetManifest.AssetManifestResp)

handler :: Environment.FlowServer API
handler = getAssetManifest

getAssetManifest ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe [Domain.Types.AssetRelease.AssetType] ->
    Environment.FlowHandler API.Types.UI.AssetManifest.AssetManifestResp
  )
getAssetManifest a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.AssetManifest.getAssetManifest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
