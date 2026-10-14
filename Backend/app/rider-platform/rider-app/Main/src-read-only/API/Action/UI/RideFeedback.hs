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
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
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
  ( TokenAuth :> "booking" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> "feedback" :> "questions"
      :> Header
           "x-language"
           Kernel.External.Types.Language
      :> Get ('[JSON]) API.Types.UI.RideFeedback.RideFeedbackQuestionsRes
      :<|> TokenAuth
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> "feedback"
      :> ReqBody
           ('[JSON])
           API.Types.UI.RideFeedback.SubmitRideFeedbackReq
      :> Post
           ('[JSON])
           API.Types.UI.RideFeedback.SubmitRideFeedbackRes
  )

handler :: Environment.FlowServer API
handler = getBookingFeedbackQuestions :<|> postBookingFeedback

getBookingFeedbackQuestions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Environment.FlowHandler API.Types.UI.RideFeedback.RideFeedbackQuestionsRes
  )
getBookingFeedbackQuestions a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.RideFeedback.getBookingFeedbackQuestions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postBookingFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    API.Types.UI.RideFeedback.SubmitRideFeedbackReq ->
    Environment.FlowHandler API.Types.UI.RideFeedback.SubmitRideFeedbackRes
  )
postBookingFeedback a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.RideFeedback.postBookingFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
