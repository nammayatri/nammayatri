module API.UI.Support
  ( API,
    handler,
    DSupport.SendIssueReq (..),
    DSupport.SendIssueRes,
  )
where

import qualified Domain.Action.UI.Support as DSupport
import Domain.Types.Person as Person
import qualified Environment as App
import EulerHS.Prelude hiding (length)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

-------- Support Flow----------
type API =
  "support"
    :> ( "sendIssue"
           :> TokenAuth
           :> ReqBody '[JSON] DSupport.SendIssueReq
           :> Post '[JSON] DSupport.SendIssueRes
       )

handler :: App.FlowServer API
handler = sendIssue

sendIssue :: Id Person.Person -> DSupport.SendIssueReq -> App.FlowHandler DSupport.SendIssueRes
sendIssue personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSupport.sendIssue personId
