{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.DeletedPerson 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.DeletedPerson
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.DeletedPerson
import qualified Kernel.Types.APISuccess



type API = (TokenAuth :> "deleted" :> "person" :> ReqBody ('[JSON]) API.Types.UI.DeletedPerson.DeletedPersonReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postDeletedPerson
postDeletedPerson :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                       Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDeletedPerson a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DeletedPerson.postDeletedPerson (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



