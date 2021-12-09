module ExternalAPI.Flow where

import qualified Beckn.Types.Core.Migration.API.Search as MigAPI
import Beckn.Types.Core.Migration.API.Types (BecknReq)
import Beckn.Types.Core.Taxi.API.Cancel as API
import Beckn.Types.Core.Taxi.API.Confirm as API
import Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import Beckn.Types.Error
import Beckn.Types.Id (ShortId (..))
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified ExternalAPI.Types as API
import GHC.Records.Extra
import Servant.Client
import Storage.Queries.Organization as Org
import Types.API.Location
import Types.Metrics (CoreMetrics)
import Utils.Common

data BAPs a = BAPs
  { metro :: a,
    cabs :: a
  }
  deriving (Generic, FromDhall)

search ::
  ( HasFlowEnv m r '["xGatewaySelector" ::: Text],
    DBFlow m r,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  API.SearchReq ->
  m API.SearchRes
search req = do
  url <- getSearchUrl
  callBecknAPIWithSignature "search" API.searchAPI url req

searchMetro ::
  ( HasFlowEnv m r '["xGatewaySelector" ::: Text],
    DBFlow m r,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro req = do
  url <- getSearchUrl
  void $ callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI url req

getSearchUrl ::
  (HasFlowEnv m r '["xGatewaySelector" ::: Text], DBFlow m r) =>
  m BaseUrl
getSearchUrl =
  asks (.xGatewaySelector)
    >>= Org.findOrgByShortId . ShortId
    >>= fromMaybeM OrgNotFound
    <&> (.callbackUrl)
    >>= fromMaybeM (OrgFieldNotPresent "callback_url")

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ConfirmRes
confirm = callBecknAPIWithSignature "confirm" API.confirmAPI

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
  m CancelRes
cancel = callBecknAPIWithSignature "cancel" API.cancelAPI

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds r m
  ) =>
  BaseUrl ->
  RatingReq ->
  m RatingRes
feedback = callBecknAPIWithSignature "feedback" API.ratingAPI

type HasBapIds r m =
  ( HasField "bapSelfIds" r (BAPs Text),
    MonadReader r m
  )

callBecknAPIWithSignature,
  callBecknAPIWithSignatureMetro ::
    ( MonadFlow m,
      CoreMetrics m,
      IsBecknAPI api req res,
      HasBapIds r m
    ) =>
    Text ->
    Proxy api ->
    BaseUrl ->
    req ->
    m res
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapSelfIds.cabs)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
callBecknAPIWithSignatureMetro a b c d = do
  bapId <- asks (.bapSelfIds.metro)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d

getHttpManagerKey :: Text -> String
getHttpManagerKey keyId = signatureAuthManagerKey <> "-" <> T.unpack keyId
