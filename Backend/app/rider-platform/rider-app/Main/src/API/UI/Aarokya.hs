module API.UI.Aarokya where

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

type API =
  "aarokya"
    :> ( "contributorToken"
           :> TokenAuth
           :> ReqBody '[JSON] DAarokya.AarokyaContributorTokenReq
           :> Post '[JSON] DAarokya.AarokyaContributorTokenRes
       )

handler :: FlowServer API
handler = contributorToken

contributorToken ::
  (Id DP.Person, Id DM.Merchant) ->
  DAarokya.AarokyaContributorTokenReq ->
  FlowHandler DAarokya.AarokyaContributorTokenRes
contributorToken (personId, merchantId) req =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    DAarokya.generateContributorToken (personId, merchantId) req
