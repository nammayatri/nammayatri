module API.Internal.RideFeedback
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.RideFeedback as Domain
import Domain.Types.Ride (Ride)
import Domain.Types.RideFeedbackResponse (RideFeedbackResponse)
import Environment
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

-- | During-ride feedback for rider-app (BAP). Authenticated with the merchant's internal API key.
type API =
  "ride"
    :> Capture "rideId" (Id Ride)
    :> "feedback"
    :> ( "questions"
           :> Header "token" Text
           :> QueryParam "language" Language
           :> Get '[JSON] Domain.RideFeedbackQuestionsRes
           :<|> Header "token" Text
             :> QueryParam "language" Language
             :> ReqBody '[JSON] Domain.SubmitRideFeedbackReq
             :> Post '[JSON] Domain.SubmitRideFeedbackRes
           :<|> Header "token" Text
             :> Get '[JSON] Domain.RideFeedbackSubmittedRes
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
handler rideId =
  (\apiKey -> withFlowHandlerAPI . Domain.getRideFeedbackQuestions rideId apiKey)
    :<|> (\apiKey mbLanguage -> withFlowHandlerAPI . Domain.postRideFeedback rideId apiKey mbLanguage)
    :<|> (withFlowHandlerAPI . Domain.getRideFeedback rideId)
    :<|> ( \responseId ->
             (\apiKey -> withFlowHandlerAPI . Domain.postActionResults rideId responseId apiKey)
               :<|> (withFlowHandlerAPI . Domain.getRetryableActions rideId responseId)
         )
