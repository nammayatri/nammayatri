module ExternalAPI.Flow where

import App.Types
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Feedback
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Error
import Beckn.Utils.Error.APIError
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
  callBecknAPI (Just signatureAuthManagerKey) Nothing url' (API.search req) "search"

confirm :: BaseUrl -> ConfirmReq -> Flow ()
confirm url req = do
  callBecknAPI (Just signatureAuthManagerKey) Nothing url (API.confirm req) "confirm"

location :: BaseUrl -> Text -> Flow GetLocationRes
location url req = do
  -- TODO: fix authentication
  callOwnAPI Nothing Nothing url (API.location req) "location"

track :: BaseUrl -> TrackTripReq -> Flow ()
track url req = do
  callBecknAPI (Just signatureAuthManagerKey) Nothing url (API.trackTrip req) "track"

cancel :: BaseUrl -> CancelReq -> Flow ()
cancel url req = do
  callBecknAPI (Just signatureAuthManagerKey) Nothing url (API.cancel req) "cancel"

status :: BaseUrl -> StatusReq -> Flow ()
status url req = do
  callBecknAPI (Just signatureAuthManagerKey) Nothing url (API.status req) "status"

feedback :: BaseUrl -> FeedbackReq -> Flow ()
feedback url req = do
  callBecknAPI (Just signatureAuthManagerKey) Nothing url (API.feedback req) "feedback"
