 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FareProduct where

import Data.OpenApi
import Domain.Types.Common
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.JSON

data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

data FareProductD (s :: UsageSafety) = FareProduct
  { id :: Id FareProduct,
    merchantId :: Id DM.Merchant,
    _type :: FareProductType,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

type FareProduct = FareProductD 'Safe

instance FromJSON (FareProductD 'Unsafe)

instance ToJSON (FareProductD 'Unsafe)

data FareProductAPIEntity = FareProductAPIEntity
  { id :: Id FareProduct,
    _type :: FareProductType,
    createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON FareProductAPIEntity where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FareProductAPIEntity where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance ToSchema FareProductAPIEntity where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

makeFareProductAPIEntity :: FareProduct -> FareProductAPIEntity
makeFareProductAPIEntity FareProduct {..} = FareProductAPIEntity {..}
