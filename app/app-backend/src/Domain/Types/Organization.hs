module Domain.Types.Organization where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Servant.API

data OrganizationType
  = PROVIDER
  | GATEWAY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData OrganizationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

data Organization = Organization
  { id :: Id Organization,
    shortId :: ShortId Organization,
    _type :: OrganizationType
  }
  deriving (Generic)
