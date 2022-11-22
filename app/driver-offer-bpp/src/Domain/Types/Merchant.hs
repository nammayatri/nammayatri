{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Merchant where

import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Domain.Types.Common
import EulerHS.Prelude hiding (id)
import Servant.API

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData Status where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

--------------------------------------------------------------------------------------

data Subscriber

data MerchantD (s :: UsageSafety) = Merchant
  { id :: Id Merchant,
    name :: Text,
    description :: Maybe Text,
    subscriberId :: ShortId Subscriber,
    uniqueKeyId :: Text,
    shortId :: ShortId Merchant,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    gstin :: Maybe Text,
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

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)

data MerchantAPIEntity = MerchantAPIEntity
  { id :: Id Merchant,
    name :: Text,
    description :: Maybe Text,
    contactNumber :: Text,
    status :: Status,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeMerchantAPIEntity :: Merchant -> MerchantAPIEntity
makeMerchantAPIEntity Merchant {..} =
  MerchantAPIEntity
    { contactNumber = fromMaybe "Unknown" $ mobileCountryCode <> mobileNumber,
      ..
    }
