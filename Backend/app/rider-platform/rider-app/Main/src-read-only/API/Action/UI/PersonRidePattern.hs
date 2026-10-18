{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PersonRidePattern
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PersonRidePattern
import qualified Control.Lens
import qualified Domain.Action.UI.PersonRidePattern
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "ridePattern" :> "summary" :> Get ('[JSON]) API.Types.UI.PersonRidePattern.RidePatternSummaryResp :<|> TokenAuth :> "ridePattern" :> "suggestions"
      :> QueryParam
           "lat"
           Kernel.Prelude.Double
      :> QueryParam "lon" Kernel.Prelude.Double
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.UI.PersonRidePattern.RideSuggestionsResp
  )

handler :: Environment.FlowServer API
handler = getRidePatternSummary :<|> getRidePatternSuggestions

getRidePatternSummary ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.PersonRidePattern.RidePatternSummaryResp
  )
getRidePatternSummary a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.PersonRidePattern.getRidePatternSummary (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getRidePatternSuggestions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Environment.FlowHandler API.Types.UI.PersonRidePattern.RideSuggestionsResp
  )
getRidePatternSuggestions a4 a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a4) $ Domain.Action.UI.PersonRidePattern.getRidePatternSuggestions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
