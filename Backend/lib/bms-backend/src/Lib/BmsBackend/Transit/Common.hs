module Lib.BmsBackend.Transit.Common where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified EulerHS.Types as ET
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Lib.BmsBackend.Types
import Network.HTTP.Types (Status (statusCode))
import Servant
import Servant.Client (ResponseF (responseBody, responseStatusCode))

data TmsCaller = TmsCaller
  { orgId :: Text,
    actorPersonId :: Text
  }
  deriving (Generic, Show)

type Transit api = "transit" :> Capture "gtfsId" Text :> api

type Scoped api = Header "x-org-id" Text :> Header "x-actor-person-id" Text :> api

scoped :: TmsCaller -> (Maybe Text -> Maybe Text -> r) -> r
scoped caller client = client (Just caller.orgId) (Just caller.actorPersonId)

data ListResponse a = ListResponse
  { items :: [a],
    total :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance (Typeable a, ToSchema [a]) => ToSchema (ListResponse a)

data BulkFailure = BulkFailure
  { row :: Int,
    error :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype ItemsResponse a = ItemsResponse
  { items :: [a]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance (Typeable a, ToSchema [a]) => ToSchema (ItemsResponse a)

data BulkResult = BulkResult
  { created :: Int,
    updated :: Int,
    failed :: [BulkFailure]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TmsError = TmsError
  { status :: Int,
    message :: Text,
    code :: Text
  }
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''TmsError

instance FromResponse TmsError where
  fromResponse resp =
    let body = decode (responseBody resp) :: Maybe Object
        field k =
          body >>= KM.lookup k >>= \case
            String s -> Just s
            _ -> Nothing
     in Just
          TmsError
            { status = statusCode (responseStatusCode resp),
              message = fromMaybe "transit service error" (field "error"),
              code = fromMaybe "TMS_ERROR" (field "code")
            }

instance IsBaseError TmsError where
  toMessage = Just . (.message)

instance IsHTTPError TmsError where
  toErrorCode = (.code)
  toHttpCode e = case e.status of
    400 -> E400
    403 -> E403
    404 -> E404
    409 -> E409
    413 -> E413
    _ -> E500

instance IsAPIError TmsError

omitNothing :: Options
omitNothing = defaultOptions {omitNothingFields = True}

callTms ::
  ( HasCallStack,
    CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasRequestId r,
    SanitizedUrl api,
    ToJSON a
  ) =>
  BmsBackendConfig ->
  Text ->
  Proxy api ->
  ET.EulerClient a ->
  m a
callTms cfg name proxy client =
  callApiUnwrappingApiError (identity @TmsError) Nothing (Just "TMS_NOT_AVAILABLE") Nothing cfg.url client name proxy
