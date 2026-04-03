{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Suspect 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import "lib-dashboard" Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Suspect
import qualified "lib-dashboard" Environment
import qualified API.Types.UI.Suspect
import qualified Kernel.Types.APISuccess



type API = (DashboardAuth ('MERCHANT_MAKER) :> "upload" :> "suspect" :> "bulk" :> ReqBody ('[JSON]) API.Types.UI.Suspect.SuspectBulkUploadReq :> Post ('[JSON])
                                                                                                                                                      API.Types.UI.Suspect.SuspectBulkUploadResp :<|> DashboardAuth ('MERCHANT_ADMIN) :> "change" :> "flag" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                       API.Types.UI.Suspect.SuspectFlagStatusChangeReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                               Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postUploadSuspectBulk :<|> postChangeFlag
postUploadSuspectBulk :: (TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.FlowHandler API.Types.UI.Suspect.SuspectBulkUploadResp)
postUploadSuspectBulk a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Suspect.postUploadSuspectBulk a2 a1
postChangeFlag :: (TokenInfo -> API.Types.UI.Suspect.SuspectFlagStatusChangeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postChangeFlag a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Suspect.postChangeFlag a2 a1



