{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import Beckn.Types.Core.ReqTypes (BecknReq (..))
import Beckn.Types.Core.Taxi.API.Call as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.Common.Context as Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallback, withBecknCallback)
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
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
  Beckn.IsBecknAPI api req res =>
  Text ->
  Proxy api ->
  Org.Organization ->
  Id SearchRequest ->
  (Common.Context -> content -> req) ->
  content ->
  m res
callBAP action api transporter searchRequestId reqConstr content = do
  searchRequest <- SearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestNotFound
  bapOrg <-
    Org.findOrganizationByShortId (ShortId searchRequest.bapId)
      >>= fromMaybeM OrgNotFound
  bapCallbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
      txnId = searchRequest.transactionId
  bppUri <- makeBppUrl (transporter.id)
  context <- buildTaxiContext txnId bapOrg.shortId.getShortId bapCallbackUrl (Just transporter.shortId.getShortId) (Just bppUri)
  Beckn.callBecknAPI (Just authKey) Nothing action api bapCallbackUrl $ reqConstr context content

callOnUpdate ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  Id SearchRequest ->
  OnUpdate.OnUpdateMessage ->
  m ()
callOnUpdate transporter searchRequestId =
  void . callBAP "on_update" API.onUpdateAPI transporter searchRequestId BecknReq

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
  void $ Beckn.callBecknAPI' Nothing (Just "UNABLE_TO_CALL") url (ET.client API.callsAPI (getId rideId)) "/v2/ride/{rideId}/call/rider"
