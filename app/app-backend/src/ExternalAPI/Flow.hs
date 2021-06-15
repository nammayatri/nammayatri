module ExternalAPI.Flow where

import App.Types
import Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.API.Feedback as API
import Beckn.Types.Core.API.Search as API
import Beckn.Types.Core.API.Status as API
import Beckn.Types.Core.API.Track as API
import Beckn.Types.Error
import Beckn.Utils.Error.APIError
import Beckn.Utils.Error.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import EulerHS.Prelude
import qualified ExternalAPI.Types as API
import Servant.Client
import Types.API.Location
import Utils.Common

search ::
  BaseUrl -> SearchReq -> Flow ()
search url req = do
  url' <-
    asks xGatewaySelector
      >>= fromMaybeM GatewaySelectorNotSet
      >>= \case
        "NSDL.BG.1" -> asks xGatewayNsdlUrl >>= fromMaybeM NSDLBaseUrlNotSet
        "JUSPAY.BG.1" -> pure url
        _ -> throwError UnsupportedGatewaySelector
  callBecknAPIWithSignature "search" API.search url' req

confirm :: BaseUrl -> ConfirmReq -> Flow ()
confirm = callBecknAPIWithSignature "confirm" API.confirm

location :: BaseUrl -> Text -> Flow GetLocationRes
location url req = do
  -- TODO: fix authentication
  callOwnAPI Nothing Nothing url (API.location req) "location"

track :: BaseUrl -> TrackTripReq -> Flow ()
track = callBecknAPIWithSignature "track" API.trackTrip

cancel :: BaseUrl -> CancelReq -> Flow ()
cancel = callBecknAPIWithSignature "cancel" API.cancel

status :: BaseUrl -> StatusReq -> Flow ()
status = callBecknAPIWithSignature "status" API.status

feedback :: BaseUrl -> FeedbackReq -> Flow ()
feedback = callBecknAPIWithSignature "feedback" API.feedback

callBecknAPIWithSignature ::
  IsBecknAPI api req =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  Flow ()
callBecknAPIWithSignature = callBecknAPI (Just signatureAuthManagerKey) Nothing
