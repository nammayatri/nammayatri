{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Places
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Places
import qualified Control.Lens
import qualified Domain.Action.UI.Places
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

type API = (TokenAuth :> "places" :> ReqBody '[JSON] API.Types.UI.Places.PlacesRequest :> Post '[JSON] API.Types.UI.Places.PlacesResponse)

handler :: Environment.FlowServer API
handler = postPlaces

postPlaces ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Places.PlacesRequest ->
    Environment.FlowHandler API.Types.UI.Places.PlacesResponse
  )
postPlaces a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Places.postPlaces (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
