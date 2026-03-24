{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Places
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Places
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.Places



type API = (TokenAuth :> "places" :> ReqBody ('[JSON]) API.Types.UI.Places.PlacesRequest :> Post ('[JSON]) API.Types.UI.Places.PlacesResponse)
handler :: Environment.FlowServer API
handler = postPlaces
postPlaces :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.Places.PlacesRequest -> Environment.FlowHandler API.Types.UI.Places.PlacesResponse)
postPlaces a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Places.postPlaces (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



