{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Cac 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Cac
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Data.Aeson



type API = (TokenAuth :> "getUiConfigs" :> QueryParam "toss" Kernel.Prelude.Int :> QueryParam "tenant" Kernel.Prelude.Text :> ReqBody ('[JSON]) Data.Aeson.Object :> Post ('[JSON]) Data.Aeson.Object)
handler :: Environment.FlowServer API
handler = postGetUiConfigs
postGetUiConfigs :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Data.Aeson.Object -> Environment.FlowHandler Data.Aeson.Object)
postGetUiConfigs a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Cac.postGetUiConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1



