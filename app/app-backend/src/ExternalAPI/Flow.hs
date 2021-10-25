module ExternalAPI.Flow where

import Beckn.Types.Core.API.Cancel as API
import Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.API.Feedback as API
import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.Migration.API.Search as MigAPI
import Beckn.Types.Core.Migration.API.Types (BecknReq)
import Beckn.Types.Error
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified ExternalAPI.Types as API
import GHC.Records.Extra
import Servant.Client
import Types.API.Location
import Types.Metrics (CoreMetrics)
import Utils.Common

data BapIds = BapIds
  { metro :: Text,
    cabs :: Text
  }
  deriving (Generic, FromDhall)

search ::
  ( HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl],
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  API.SearchReq ->
  m ()
search url req = do
  url' <- getSearchUrl url
  callBecknAPIWithSignature "search" API.search url' req

searchMetro ::
  ( HasFlowEnv m r ["xGatewaySelector" ::: Maybe Text, "xGatewayNsdlUrl" ::: Maybe BaseUrl],
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro url req = do
  url' <- getSearchUrl url
  callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI url' req

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
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ()
confirm = callBecknAPIWithSignature "confirm" API.confirm

location ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  Text ->
  m GetLocationRes
location url req = do
  -- TODO: fix authentication
  callOwnAPI Nothing Nothing url (API.location req) "location"

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  CancelReq ->
  m ()
cancel = callBecknAPIWithSignature "cancel" API.cancel

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  FeedbackReq ->
  m ()
feedback = callBecknAPIWithSignature "feedback" API.feedback

type HasBapIds r m =
  ( HasField "bapSelfIds" r BapIds,
    MonadReader r m
  )

callBecknAPIWithSignature,
  callBecknAPIWithSignatureMetro ::
    ( MonadFlow m,
      CoreMetrics m,
      IsBecknAPI api req,
      HasBapIds r m
    ) =>
    Text ->
    Proxy api ->
    BaseUrl ->
    req ->
    m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapSelfIds.cabs)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
callBecknAPIWithSignatureMetro a b c d = do
  bapId <- asks (.bapSelfIds.metro)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d

getHttpManagerKey :: Text -> String
getHttpManagerKey keyId = signatureAuthManagerKey <> "-" <> T.unpack keyId
