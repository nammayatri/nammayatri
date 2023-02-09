module Domain.Types.BlackListOrg where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Types.Id
import Servant.API

data BlackListOrgType
  = PROVIDER
  | APP
  | GATEWAY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData BlackListOrgType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

data BlackListOrgD (s :: UsageSafety) = BlackListOrg
  { id :: Id BlackListOrg,
    shortId :: ShortId BlackListOrg,
    _type :: BlackListOrgType
  }
  deriving (Generic)

type BlackListOrg = BlackListOrgD 'Safe

instance FromJSON (BlackListOrgD 'Unsafe)

instance ToJSON (BlackListOrgD 'Unsafe)
