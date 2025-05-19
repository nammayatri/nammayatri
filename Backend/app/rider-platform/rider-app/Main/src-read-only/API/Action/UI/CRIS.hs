{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CRIS
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CRIS
import qualified Control.Lens
import qualified Domain.Action.UI.CRIS as Domain.Action.UI.CRIS
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

type API = (TokenAuth :> "cris" :> "getSDKData" :> ReqBody '[JSON] API.Types.UI.CRIS.GetSDKDataRequest :> Post '[JSON] API.Types.UI.CRIS.GetSDKDataResponse)

handler :: Environment.FlowServer API
handler = postCrisGetSDKData

postCrisGetSDKData ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.CRIS.GetSDKDataRequest ->
    Environment.FlowHandler API.Types.UI.CRIS.GetSDKDataResponse
  )
postCrisGetSDKData a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CRIS.postCrisGetSDKData (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
