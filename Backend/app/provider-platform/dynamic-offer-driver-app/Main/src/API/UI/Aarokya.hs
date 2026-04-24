module API.UI.Aarokya where

import qualified Domain.Action.UI.Aarokya as DAarokya
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Utils.Logging
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "aarokya"
    :> ( "generateToken"
           :> TokenAuth
           :> Post '[JSON] DAarokya.AarokyaTokenRes
       )

handler :: FlowServer API
handler = generateToken

generateToken ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  FlowHandler DAarokya.AarokyaTokenRes
generateToken (personId, merchantId, merchantOpCityId) =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    DAarokya.generateToken (personId, merchantId, merchantOpCityId)
