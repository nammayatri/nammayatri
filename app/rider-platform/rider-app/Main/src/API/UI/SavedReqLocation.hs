module API.UI.SavedReqLocation
  ( API,
    handler,
    DSavedReqLocation.CreateSavedReqLocationReq (..),
    DSavedReqLocation.SavedReqLocationsListRes (..),
  )
where

import qualified Domain.Action.UI.SavedReqLocation as DSavedReqLocation
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (state)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "savedLocation"
    :> ( TokenAuth
           :> ReqBody '[JSON] DSavedReqLocation.CreateSavedReqLocationReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> "list"
             :> Get '[JSON] DSavedReqLocation.SavedReqLocationsListRes
           :<|> TokenAuth
             :> Capture "tag" Text
             :> Delete '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  createSavedReqLocation
    :<|> getSavedReqLocations
    :<|> deleteSavedReqLocation

createSavedReqLocation :: Id Person.Person -> DSavedReqLocation.CreateSavedReqLocationReq -> FlowHandler APISuccess.APISuccess
createSavedReqLocation riderId = withFlowHandlerAPI . withPersonIdLogTag riderId . DSavedReqLocation.createSavedReqLocation riderId

getSavedReqLocations :: Id Person.Person -> FlowHandler DSavedReqLocation.SavedReqLocationsListRes
getSavedReqLocations riderId = withFlowHandlerAPI . withPersonIdLogTag riderId $ DSavedReqLocation.getSavedReqLocations riderId

deleteSavedReqLocation :: Id Person.Person -> Text -> FlowHandler APISuccess.APISuccess
deleteSavedReqLocation riderId = withFlowHandlerAPI . withPersonIdLogTag riderId . DSavedReqLocation.deleteSavedReqLocation riderId
