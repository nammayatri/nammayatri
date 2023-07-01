module SharedLogic.CallBPPInternal where

import Domain.Types.FeedbackForm
import EulerHS.Types (EulerClient, client)
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common hiding (Error)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Text,
    customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type LinkRefereeAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "referee"
    :> Header "token" Text
    :> ReqBody '[JSON] RefereeLinkInfoReq
    :> Post '[JSON] APISuccess

linkRefereeClient :: Text -> Maybe Text -> RefereeLinkInfoReq -> EulerClient APISuccess
linkRefereeClient = client likeRefereeApi

likeRefereeApi :: Proxy LinkRefereeAPI
likeRefereeApi = Proxy

linkReferee ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  m APISuccess
linkReferee apiKey internalUrl merchantId referralCode phoneNumber countryCode = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (linkRefereeClient merchantId (Just apiKey) (RefereeLinkInfoReq referralCode phoneNumber countryCode)) "LinkReferee" likeRefereeApi

type FeedbackFormAPI =
  "internal"
    :> "beckn"
    :> "booking"
    :> ( "feedback"
           :> ReqBody '[JSON] FeedbackFormReq
           :> Post '[JSON] APISuccess
       )

feedbackFormClient :: FeedbackFormReq -> EulerClient APISuccess
feedbackFormClient = client (Proxy @FeedbackFormAPI)

feedbackFormApi :: Proxy FeedbackFormAPI
feedbackFormApi = Proxy

feedbackForm ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  FeedbackFormReq ->
  m APISuccess
feedbackForm internalUrl request = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (feedbackFormClient request) "FeedbackForm" feedbackFormApi
