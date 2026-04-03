{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.SpecialZoneQueue 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.SpecialZoneQueue
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.SpecialZoneQueue
import qualified Domain.Types.SpecialZoneQueueRequest
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "specialZoneQueue" :> "request" :> Get ('[JSON])
                                                                API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes :<|> TokenAuth :> "specialZoneQueue" :> "request" :> Capture "requestId"
                                                                                                                                                                                          (Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest) :> "respond" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                                    API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRespondReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                     Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = getSpecialZoneQueueRequest :<|> postSpecialZoneQueueRequestRespond
getSpecialZoneQueueRequest :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes)
getSpecialZoneQueueRequest a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialZoneQueue.getSpecialZoneQueueRequest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
postSpecialZoneQueueRequestRespond :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                        Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                        Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest -> API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRespondReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueRequestRespond a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialZoneQueue.postSpecialZoneQueueRequestRespond (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1



