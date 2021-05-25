module External.Gateway.Flow where

import App.Types
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Feedback
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import Types.API.Location
import Utils.Common

search ::
  BaseUrl -> SearchReq -> Flow AckResponse
search url req = do
  mGatewaySelector <- xGatewaySelector <$> ask
  case mGatewaySelector of
    Just "NSDL.BG.1" -> do
      nsdlBaseUrl <- ask <&> xGatewayNsdlUrl >>= fromMaybeM NSDLBaseUrlNotSet
      callAPI' (Just signatureAuthManagerKey) nsdlBaseUrl (API.nsdlSearch req) "search"
        >>= fromEitherM (ExternalAPICallError nsdlBaseUrl)
    Just "JUSPAY.BG.1" ->
      callAPI' (Just signatureAuthManagerKey) url (API.search req) "search"
        >>= fromEitherM (ExternalAPICallError url)
    _ -> throwError GatewaySelectorNotSet

confirm :: BaseUrl -> ConfirmReq -> Flow AckResponse
confirm url req = do
  callAPI' (Just signatureAuthManagerKey) url (API.confirm req) "confirm"
    >>= fromEitherM (ExternalAPICallError url)

location :: BaseUrl -> Text -> Flow GetLocationRes
location url req = do
  -- TODO: fix authentication
  callAPI' Nothing url (API.location req) "location"
    >>= fromEitherM (ExternalAPICallError url)

track :: BaseUrl -> TrackTripReq -> Flow AckResponse
track url req = do
  callAPI' (Just signatureAuthManagerKey) url (API.trackTrip req) "track"
    >>= fromEitherM (ExternalAPICallError url)

cancel :: BaseUrl -> CancelReq -> Flow AckResponse
cancel url req = do
  callAPI' (Just signatureAuthManagerKey) url (API.cancel req) "cancel"
    >>= fromEitherM (ExternalAPICallError url)

status :: BaseUrl -> StatusReq -> Flow AckResponse
status url req = do
  callAPI' (Just signatureAuthManagerKey) url (API.status req) "status"
    >>= fromEitherM (ExternalAPICallError url)

feedback :: BaseUrl -> FeedbackReq -> Flow AckResponse
feedback url req = do
  callAPI' (Just signatureAuthManagerKey) url (API.feedback req) "feedback"
    >>= fromEitherM (ExternalAPICallError url)
