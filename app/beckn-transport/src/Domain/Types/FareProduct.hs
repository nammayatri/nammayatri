module Domain.Types.FareProduct where

import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Domain.Types.Organization as DOrg
import EulerHS.Prelude hiding (id)

data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq)

data FareProduct = FareProduct
  { id :: Id FareProduct,
    organizationId :: Id DOrg.Organization,
    enabled :: Bool,
    _type :: FareProductType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
