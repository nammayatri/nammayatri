{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Miscellaneous 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Miscellaneous
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.Miscellaneous



type API = (TokenAuth :> "misc" :> "testScanQR" :> ReqBody ('[JSON]) API.Types.UI.Miscellaneous.QRScanTestReq :> Post ('[JSON]) API.Types.UI.Miscellaneous.QRScanTestResp)
handler :: Environment.FlowServer API
handler = postMiscTestScanQR
postMiscTestScanQR :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                        Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.Miscellaneous.QRScanTestReq -> Environment.FlowHandler API.Types.UI.Miscellaneous.QRScanTestResp)
postMiscTestScanQR a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Miscellaneous.postMiscTestScanQR (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



