{-# LANGUAGE OverloadedLabels #-}

module ExternalAPI.Flow where

import Beckn.Types.Core.ReqTypes (BecknReq (..))
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.Common.Context as Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Control.Lens.Operators ((?~))
import qualified Data.Text as T
import EulerHS.Prelude
import Storage.Queries.SearchRequest as SearchRequest
import Tools.Metrics (CoreMetrics)
import Types.Error
import Types.Storage.Organization as Org
import Types.Storage.SearchRequest as SearchRequest
import Utils.Auth
import Utils.Common

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallbackMig api callback_success m
withCallback = withCallback' identity

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  Org.Organization ->
  WithBecknCallbackMig api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- makeBppUrl (transporter.id)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ transporter.shortId.getShortId
  withBecknCallbackMig doWithCallback (Just authKey) action api context' cbUrl f

callBAP ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Beckn.IsBecknAPI api req res =>
  Common.Action ->
  Proxy api ->
  Org.Organization ->
  Id SearchRequest ->
  (Common.Context -> content -> req) ->
  content ->
  m res
callBAP action api transporter searchRequestId reqConstr content = do
  searchRequest <- SearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestNotFound
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
      txnId = searchRequest.transactionId
  bppUri <- makeBppUrl (transporter.id)
  context <- buildTaxiContext action txnId bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  Beckn.callBecknAPI (Just authKey) Nothing (show action) api bapUri $ reqConstr context content

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
  void . callBAP Common.ON_UPDATE API.onUpdateAPI transporter searchRequestId BecknReq

makeBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Org.Organization ->
  m BaseUrl
makeBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)
