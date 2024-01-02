{-# LANGUAGE ApplicativeDo #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}

module Domain.Types.Merchant where

import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    serverNames :: [DSN.ServerName],
    is2faMandatory :: Bool,
    defaultOperatingCity :: City.City,
    supportedOperatingCities :: [City.City],
    domain :: Maybe Text,
    website :: Maybe Text,
    authToken :: Maybe Text,
    createdAt :: UTCTime,
    enabled :: Maybe Bool
  }
  deriving (Generic)

data MerchantAPIEntity = MerchantAPIEntity
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    domain :: Maybe Text,
    website :: Maybe Text,
    authToken :: Maybe Text,
    supportedOperatingCities :: [City.City],
    defaultOperatingCity :: City.City,
    adminList :: [(Text, Text)],
    enabled :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mkMerchantAPIEntity :: Merchant -> [(Text, Text)] -> MerchantAPIEntity
mkMerchantAPIEntity merchant adminList = do
  MerchantAPIEntity
    { id = merchant.id,
      shortId = merchant.shortId,
      domain = merchant.domain,
      website = merchant.website,
      supportedOperatingCities = merchant.supportedOperatingCities,
      defaultOperatingCity = merchant.defaultOperatingCity,
      authToken = merchant.authToken,
      adminList = adminList,
      enabled = merchant.enabled
    }
