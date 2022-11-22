module Domain.Types.FarePolicy.FareProduct where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import Beckn.Utils.JSON
import Data.OpenApi
import Domain.Types.Common
import qualified Domain.Types.Merchant as DM

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
