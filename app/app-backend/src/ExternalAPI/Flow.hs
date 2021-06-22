module ExternalAPI.Flow where

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
import GHC.Records.Extra
import Servant.Client
import Types.API.Location
import Utils.Common

search ::
  HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl] =>
  BaseUrl ->
  SearchReq ->
  m ()
search url req = do
  url' <-
    asks (.xGatewaySelector)
      >>= fromMaybeM GatewaySelectorNotSet
      >>= \case
        "NSDL.BG.1" -> asks (.xGatewayNsdlUrl) >>= fromMaybeM NSDLBaseUrlNotSet
        "JUSPAY.BG.1" -> pure url
        _ -> throwError UnsupportedGatewaySelector
  callBecknAPIWithSignature "search" API.search url' req

confirm ::
  MonadFlow m =>
  BaseUrl ->
  ConfirmReq ->
  m ()
confirm = callBecknAPIWithSignature "confirm" API.confirm

location ::
  MonadFlow m =>
  BaseUrl ->
  Text ->
  m GetLocationRes
location url req = do
  -- TODO: fix authentication
  callOwnAPI Nothing Nothing url (API.location req) "location"

track ::
  MonadFlow m =>
  BaseUrl ->
  TrackTripReq ->
  m ()
track = callBecknAPIWithSignature "track" API.trackTrip

cancel ::
  MonadFlow m =>
  BaseUrl ->
  CancelReq ->
  m ()
cancel = callBecknAPIWithSignature "cancel" API.cancel

status ::
  MonadFlow m =>
  BaseUrl ->
  StatusReq ->
  m ()
status = callBecknAPIWithSignature "status" API.status

feedback ::
  MonadFlow m =>
  BaseUrl ->
  FeedbackReq ->
  m ()
feedback = callBecknAPIWithSignature "feedback" API.feedback

callBecknAPIWithSignature ::
  ( MonadFlow m,
    IsBecknAPI api req
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature = callBecknAPI (Just signatureAuthManagerKey) Nothing
