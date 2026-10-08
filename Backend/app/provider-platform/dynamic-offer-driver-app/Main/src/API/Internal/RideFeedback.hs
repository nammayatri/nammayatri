module API.Internal.RideFeedback
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.RideFeedback as Domain
import Domain.Types.Booking (Booking)
import Domain.Types.RideFeedbackResponse (RideFeedbackResponse)
import Environment
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

-- | Ride feedback for rider-app (BAP), keyed by this platform's booking id. Authenticated with the merchant's internal API key.
type API =
  "booking"
    :> Capture "bookingId" (Id Booking)
    :> "feedback"
    :> ( "questions"
           :> Header "token" Text
           :> QueryParam "language" Language
           :> Get '[JSON] Domain.RideFeedbackQuestionsRes
           :<|> Header "token" Text
             :> ReqBody '[JSON] Domain.SubmitRideFeedbackReq
             :> Post '[JSON] Domain.SubmitRideFeedbackRes
           :<|> "response"
             :> Capture "responseId" (Id RideFeedbackResponse)
             :> ( "actionResults"
                    :> Header "token" Text
                    :> ReqBody '[JSON] Domain.ReportActionResultsReq
                    :> Post '[JSON] APISuccess
                    :<|> "retryableActions"
                      :> Header "token" Text
                      :> Get '[JSON] Domain.RetryableActionsRes
                )
       )

handler :: FlowServer API
handler bookingId =
  (\apiKey -> withFlowHandlerAPI . Domain.getRideFeedbackQuestions bookingId apiKey)
    :<|> (\apiKey -> withFlowHandlerAPI . Domain.postRideFeedback bookingId apiKey)
    :<|> ( \responseId ->
             (\apiKey -> withFlowHandlerAPI . Domain.postActionResults bookingId responseId apiKey)
               :<|> (withFlowHandlerAPI . Domain.getRetryableActions bookingId responseId)
         )
