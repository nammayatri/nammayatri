{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import Beckn.Types.Core.API.Call as API
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Error
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallback, withBecknCallback)
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Control.Arrow ((>>>))
import Control.Lens.Operators ((?~))
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Storage.Queries.Organization as Org
import Storage.Queries.SearchRequest as SearchRequest
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.Storage.Organization as Org
import qualified Types.Storage.Ride as SRide
import Types.Storage.SearchRequest as SearchRequest
import Utils.Auth
import Utils.Common

getGatewayUrl ::
  ( DBFlow m r,
    HasField "xGatewaySelector" r Text
  ) =>
  m BaseUrl
getGatewayUrl =
  asks (.xGatewaySelector)
    >>= Org.findOrgByShortId . ShortId
    >>= fromMaybeM OrgNotFound
    <&> (.callbackUrl)
    >>= fromMaybeM (OrgFieldNotPresent "callback_url")

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallback api callback_success m
withCallback = withCallback' identity

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallback api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  let context' = context & #bpp_uri ?~ bppUri
  withBecknCallback doWithCallback (Just authKey) action api context' cbUrl f

callBAP ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Beckn.IsBecknAPI api (CallbackReq req) =>
  Text ->
  Proxy api ->
  Org.Organization ->
  Id SearchRequest ->
  Either Error req ->
  m ()
callBAP action api transporter searchRequestId contents = do
  searchRequest <- SearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestNotFound
  bapCallbackUrl <-
    (searchRequest.bapId)
      & (Id >>> Org.findOrganizationById)
      >>= fromMaybeM OrgNotFound
      >>= ((.callbackUrl) >>> fromMaybeM (OrgFieldNotPresent "callback_url"))
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
      txnId = searchRequest.transactionId
  bppUri <- makeBppUrl (transporter.id)
  context <- buildContext action txnId (Just bapCallbackUrl) (Just bppUri)
  Beckn.callBecknAPI (Just authKey) Nothing action api bapCallbackUrl $
    CallbackReq {contents, context}

makeBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Org.Organization ->
  m BaseUrl
makeBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

initiateCall ::
  ( HasFlowEnv m r '["xAppUri" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  m ()
initiateCall rideId = do
  url <- asks (.xAppUri)
  Beckn.callBecknAPI' Nothing (Just "UNABLE_TO_CALL") url (ET.client API.callsAPI (getId rideId)) "/v2/ride/{rideId}/call/rider"
