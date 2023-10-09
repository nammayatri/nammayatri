{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Merchant where

import Domain.Types.Common
import Kernel.Prelude
import Kernel.Types.Base64 (Base64)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.Time (Seconds)

data MerchantD (s :: UsageSafety) = Merchant
  { id :: Id Merchant,
    subscriberId :: ShortId Subscriber,
    shortId :: ShortId Merchant,
    name :: Text,
    city :: Context.City,
    country :: Context.Country,
    geofencingConfig :: GeofencingConfig,
    gatewayUrl :: BaseUrl,
    registryUrl :: BaseUrl,
    bapId :: Text,
    bapUniqueKeyId :: Text,
    driverOfferBaseUrl :: BaseUrl,
    driverOfferApiKey :: Text,
    driverOfferMerchantId :: Text,
    geoHashPrecisionValue :: Int,
    minimumDriverRatesCount :: Int,
    signingPublicKey :: Base64,
    cipherText :: Maybe Base64,
    signatureExpiry :: Int,
    distanceWeightage :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    timeDiffFromUtc :: Seconds,
    isAvoidToll :: Bool,
    aadhaarVerificationTryLimit :: Int,
    aadhaarKeyExpiryTime :: Seconds
  }
  deriving (Generic, Show)

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)
