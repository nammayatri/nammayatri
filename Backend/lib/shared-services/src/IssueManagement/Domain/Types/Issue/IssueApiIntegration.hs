module IssueManagement.Domain.Types.Issue.IssueApiIntegration where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.Beam.Lib.UtilsTH
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

-- | A declared external/internal HTTP lookup that chat flows can execute
-- mid-conversation. 'headersJson' and 'responseFieldsJson' are JSON-encoded
-- blobs (see the rich specs in IssueManagement.Common.Dashboard.Issue) so the
-- table needs no custom Beam instances - same trick as
-- IssueReport.additionalTicketIds.
-- | Scoped per merchant (not per operating city): integrations are
-- infrastructure endpoints, not localized content, so one declaration serves
-- every city. City-specific behaviour goes through {{placeholders}} instead.
data IssueApiIntegration = IssueApiIntegration
  { id :: Id IssueApiIntegration,
    merchantId :: Id Merchant,
    name :: Text,
    description :: Maybe Text,
    -- | External: urlTemplate is a full URL, author supplies headers.
    -- InternalRider/InternalDriver: urlTemplate is only the endpoint tail after
    -- /dashboard/{merchantShortId}/{city}/ on this app itself; the engine
    -- builds the base from selfBaseUrl and attaches the config dashboardToken,
    -- so internal lookups need no author-managed credentials and cannot point
    -- outside the app.
    kind :: IntegrationKind,
    method :: ApiMethod,
    urlTemplate :: Text,
    headersJson :: Maybe Text,
    bodyTemplate :: Maybe Text,
    timeoutMs :: Int,
    responseFieldsJson :: Text,
    isActive :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

data ApiMethod = GET | POST
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

data IntegrationKind = External | InternalRider | InternalDriver
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema, Read, Ord)

-- | One HTTP header on a declared lookup. Values may contain {{placeholders}}.
data ApiHeaderSpec = ApiHeaderSpec
  { key :: Text,
    value :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

-- | One field extracted from the lookup's JSON response into a flow variable.
-- 'path' is a dot-path into the response object, e.g. "data.ride.status".
data ResponseFieldSpec = ResponseFieldSpec
  { name :: Text,
    path :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

$(mkBeamInstancesForEnum ''ApiMethod)

$(mkBeamInstancesForEnum ''IntegrationKind)
