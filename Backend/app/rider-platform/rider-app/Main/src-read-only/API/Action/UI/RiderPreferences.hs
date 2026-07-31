{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RiderPreferences
  ( API,
    handler,
  )
where

import qualified API.Types.UI.RiderPreferences
import qualified Control.Lens
import qualified Domain.Action.UI.RiderPreferences
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RiderPreferences
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "riderPreference" :> ReqBody ('[JSON]) API.Types.UI.RiderPreferences.RiderPreferenceReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "riderPreference"
      :> QueryParam "sourceLat" Kernel.Prelude.Double
      :> QueryParam
           "sourceLon"
           Kernel.Prelude.Double
      :> Get
           ('[JSON])
           API.Types.UI.RiderPreferences.RiderPreferencesResp
      :<|> TokenAuth
      :> "riderPreference"
      :> "all"
      :> Get
           ('[JSON])
           API.Types.UI.RiderPreferences.AllRiderPreferencesResp
      :<|> TokenAuth
      :> "riderPreference"
      :> Capture
           "preferenceId"
           (Kernel.Types.Id.Id Domain.Types.RiderPreferences.RiderPreferences)
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postRiderPreference :<|> getRiderPreference :<|> getAllRiderPreferences :<|> deleteRiderPreference

postRiderPreference ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.RiderPreferences.RiderPreferenceReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postRiderPreference a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderPreferences.postRiderPreference (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getRiderPreference ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Environment.FlowHandler API.Types.UI.RiderPreferences.RiderPreferencesResp
  )
getRiderPreference a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderPreferences.getRiderPreference (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getAllRiderPreferences ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.RiderPreferences.AllRiderPreferencesResp
  )
getAllRiderPreferences a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderPreferences.getAllRiderPreferences (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

deleteRiderPreference ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.RiderPreferences.RiderPreferences ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
deleteRiderPreference a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RiderPreferences.deleteRiderPreference (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
