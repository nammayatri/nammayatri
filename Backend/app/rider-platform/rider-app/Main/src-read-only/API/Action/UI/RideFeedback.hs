{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RideFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.UI.RideFeedback
import qualified Control.Lens
import qualified Domain.Action.UI.RideFeedback
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "ride" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "feedback" :> "questions" :> Header "x-language" Kernel.External.Types.Language
      :> Get
           ('[JSON])
           API.Types.UI.RideFeedback.RideFeedbackQuestionsRes
      :<|> TokenAuth
      :> "ride"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> "feedback"
      :> Header
           "x-language"
           Kernel.External.Types.Language
      :> ReqBody
           ('[JSON])
           API.Types.UI.RideFeedback.SubmitRideFeedbackReq
      :> Post
           ('[JSON])
           API.Types.UI.RideFeedback.SubmitRideFeedbackRes
      :<|> TokenAuth
      :> "ride"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> "feedback"
      :> Get
           ('[JSON])
           API.Types.UI.RideFeedback.RideFeedbackSubmittedRes
  )

handler :: Environment.FlowServer API
handler = getRideFeedbackQuestions :<|> postRideFeedback :<|> getRideFeedback

getRideFeedbackQuestions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Environment.FlowHandler API.Types.UI.RideFeedback.RideFeedbackQuestionsRes
  )
getRideFeedbackQuestions a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.RideFeedback.getRideFeedbackQuestions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postRideFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    API.Types.UI.RideFeedback.SubmitRideFeedbackReq ->
    Environment.FlowHandler API.Types.UI.RideFeedback.SubmitRideFeedbackRes
  )
postRideFeedback a4 a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a4) $ Domain.Action.UI.RideFeedback.postRideFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getRideFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler API.Types.UI.RideFeedback.RideFeedbackSubmittedRes
  )
getRideFeedback a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.RideFeedback.getRideFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
