{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Merchant where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import Domain.Types.Common
import EulerHS.Prelude hiding (id)
import Kernel.Prelude (BaseUrl)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.TH (mkFromHttpInstanceForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''Status)

$(mkFromHttpInstanceForEnum ''Status)

--------------------------------------------------------------------------------------

data Subscriber

data MerchantD (s :: UsageSafety) = Merchant
  { id :: Id Merchant,
    name :: Text,
    description :: Maybe Text,
    subscriberId :: ShortId Subscriber,
    uniqueKeyId :: Text,
    shortId :: ShortId Merchant,
    city :: Context.City,
    country :: Context.Country,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    gstin :: Maybe Text,
    fromTime :: Maybe UTCTime,
    toTime :: Maybe UTCTime,
    headCount :: Maybe Int,
    status :: Status,
    verified :: Bool,
    enabled :: Bool,
    internalApiKey :: Text,
    geoHashPrecisionValue :: Int,
    minimumDriverRatesCount :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    geofencingConfig :: GeofencingConfig,
    info :: Maybe Text,
    registryUrl :: BaseUrl
  }
  deriving (Generic, Show)

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
