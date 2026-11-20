{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ShuttleInterest
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ShuttleInterest
import qualified Control.Lens
import qualified Domain.Action.UI.ShuttleInterest
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API = TokenAuth :> ("shuttle" :> ("interest" :> (ReqBody ('[JSON]) API.Types.UI.ShuttleInterest.ShuttleInterestReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)))

handler :: Environment.FlowServer API
handler = postShuttleInterest

postShuttleInterest ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  (API.Types.UI.ShuttleInterest.ShuttleInterestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postShuttleInterest a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.ShuttleInterest.postShuttleInterest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
