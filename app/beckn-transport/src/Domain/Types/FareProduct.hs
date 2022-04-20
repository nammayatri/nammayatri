module Domain.Types.FareProduct where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import Beckn.Utils.JSON
import Data.OpenApi
import qualified Domain.Types.Organization as DOrg

data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

data FareProduct = FareProduct
  { id :: Id FareProduct,
    organizationId :: Id DOrg.Organization,
    _type :: FareProductType,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

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
