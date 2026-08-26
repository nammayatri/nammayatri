module API.UI.Aarokya where

import qualified Data.Aeson as A
import qualified Domain.Action.UI.Aarokya as DAarokya
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Logging
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

-- Response is Aarokya's raw JSON body ('A.Value'), forwarded verbatim.
type API =
  "aarokya"
    :> ( "contributorToken"
           :> TokenAuth
           :> ReqBody '[JSON] DAarokya.AarokyaContributorTokenReq
           :> Post '[JSON] A.Value
       )

handler :: FlowServer API
handler = contributorToken

contributorToken ::
  (Id DP.Person, Id DM.Merchant) ->
  DAarokya.AarokyaContributorTokenReq ->
  FlowHandler A.Value
contributorToken (personId, merchantId) req =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    DAarokya.generateContributorToken (personId, merchantId) req
