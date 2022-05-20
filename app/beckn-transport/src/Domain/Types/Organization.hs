{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Organization where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import EulerHS.Prelude hiding (id)
import Servant.API

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

--------------------------------------------------------------------------------------

data OrganizationType
  = PROVIDER
  | APP
  | GATEWAY
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData OrganizationType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data OrganizationDomain
  = MOBILITY
  | LOGISTICS
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  deriving (Show, Eq, Read, Generic)

instance ToJSON OrganizationDomain where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON OrganizationDomain where
  parseJSON = genericParseJSON constructorsWithHyphens

instance FromHttpApiData OrganizationDomain where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data Organization = Organization
  { id :: Id Organization,
    name :: Text,
    description :: Maybe Text,
    shortId :: ShortId Organization,
    uniqueKeyId :: Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    gstin :: Maybe Text,
    _type :: OrganizationType,
    domain :: Maybe OrganizationDomain,
    fromTime :: Maybe UTCTime,
    toTime :: Maybe UTCTime,
    headCount :: Maybe Int,
    status :: Status,
    verified :: Bool,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    info :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data OrganizationAPIEntity = OrganizationAPIEntity
  { name :: Text,
    description :: Maybe Text,
    contactNumber :: Text,
    status :: Status,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeOrganizationAPIEntity :: Organization -> OrganizationAPIEntity
makeOrganizationAPIEntity Organization {..} =
  OrganizationAPIEntity
    { contactNumber = fromMaybe "Unknown" $ mobileCountryCode <> mobileNumber,
      ..
    }
