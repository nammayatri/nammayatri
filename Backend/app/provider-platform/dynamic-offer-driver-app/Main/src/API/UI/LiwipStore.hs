module API.UI.LiwipStore where

import qualified Data.Aeson as A
import qualified Domain.Action.UI.LiwipStore as DLiwip
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
  "liwip"
    :> ( "session"
           :> TokenAuth
           :> Post '[JSON] A.Value
       )

handler :: FlowServer API
handler = getSession

getSession ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  FlowHandler A.Value
getSession authInfo@(personId, _, _) =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    DLiwip.getLiwipSession authInfo
