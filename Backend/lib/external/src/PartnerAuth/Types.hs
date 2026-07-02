module PartnerAuth.Types
  ( PartnerAuthService (..),
    availablePartnerAuthServices,
    parsePartnerAuthService,
    PartnerUserDetails (..),
  )
where

import qualified Data.Text as T
import Kernel.Prelude

-- | The set of supported partner embedded-auth providers.
-- Add a new constructor here (and a matching provider module + Interface case)
-- to onboard another partner. BHIM is the first provider.
data PartnerAuthService = BHIM
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

availablePartnerAuthServices :: [PartnerAuthService]
availablePartnerAuthServices = [BHIM]

-- | Parse the provider from the request path segment (case-insensitive).
-- e.g. "bhim" -> Just BHIM. Unknown providers return Nothing.
parsePartnerAuthService :: Text -> Maybe PartnerAuthService
parsePartnerAuthService provider = case T.toLower provider of
  "bhim" -> Just BHIM
  _ -> Nothing

-- | Provider-agnostic user details returned by a partner after token verification.
data PartnerUserDetails = PartnerUserDetails
  { name :: Text,
    mobileNumber :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
