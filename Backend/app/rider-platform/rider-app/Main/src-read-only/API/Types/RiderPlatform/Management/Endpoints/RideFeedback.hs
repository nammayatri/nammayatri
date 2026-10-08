{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.RideFeedback where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("rideFeedback" :> PostRideFeedbackBookingResponseRetryActions)

type PostRideFeedbackBookingResponseRetryActions =
  ( "booking" :> Capture "bookingId" Kernel.Prelude.Text :> "response" :> Capture "responseId" Kernel.Prelude.Text :> "retryActions"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

newtype RideFeedbackAPIs = RideFeedbackAPIs {postRideFeedbackBookingResponseRetryActions :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkRideFeedbackAPIs :: (Client EulerHS.Types.EulerClient API -> RideFeedbackAPIs)
mkRideFeedbackAPIs rideFeedbackClient = (RideFeedbackAPIs {..})
  where
    postRideFeedbackBookingResponseRetryActions = rideFeedbackClient

data RideFeedbackUserActionType
  = POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON RideFeedbackUserActionType where
  toJSON (POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS) = Data.Aeson.String "POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS"

instance FromJSON RideFeedbackUserActionType where
  parseJSON (Data.Aeson.String "POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS") = pure POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS
  parseJSON _ = fail "POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS expected"

$(Data.Singletons.TH.genSingletons [(''RideFeedbackUserActionType)])
