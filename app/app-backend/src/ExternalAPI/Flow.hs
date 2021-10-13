module ExternalAPI.Flow where

import Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.API.Feedback as API
import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.Migration.API.Search as MigAPI
import Beckn.Types.Core.Migration.API.Types (BecknReq)
import Beckn.Types.Error
import Beckn.Utils.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import EulerHS.Prelude
import qualified ExternalAPI.Types as API
import GHC.Records.Extra
import Servant.Client
import Types.API.Location
import Types.Metrics (CoreMetrics)
import Utils.Common

search ::
  ( HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl],
    CoreMetrics m
  ) =>
  BaseUrl ->
  API.SearchReq ->
  m ()
search url req = do
  url' <- getSearchUrl url
  callBecknAPIWithSignature "search" API.search url' req

searchMig ::
  ( HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl],
    CoreMetrics m
  ) =>
  BaseUrl ->
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMig url req = do
  url' <- getSearchUrl url
  callBecknAPIWithSignature "search" MigAPI.searchAPI url' req

getSearchUrl ::
  (HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl]) =>
  BaseUrl ->
  m BaseUrl
getSearchUrl url =
  asks (.xGatewaySelector)
    >>= fromMaybeM GatewaySelectorNotSet
    >>= \case
      "NSDL.BG.1" -> asks (.xGatewayNsdlUrl) >>= fromMaybeM NSDLBaseUrlNotSet
      "JUSPAY.BG.1" -> pure url
      _ -> throwError UnsupportedGatewaySelector

confirm ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ()
confirm = callBecknAPIWithSignature "confirm" API.confirm

location ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Text ->
  m GetLocationRes
location url req = do
  -- TODO: fix authentication
  callOwnAPI Nothing Nothing url (API.location req) "location"

cancel ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  CancelReq ->
  m ()
cancel = callBecknAPIWithSignature "cancel" API.cancel

feedback ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  FeedbackReq ->
  m ()
feedback = callBecknAPIWithSignature "feedback" API.feedback

callBecknAPIWithSignature ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature = callBecknAPI (Just signatureAuthManagerKey) Nothing
