module Domain.Types.Quote where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Time
import qualified Domain.Types.Organization as DOrg
import Domain.Types.Products (Products)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSR.SearchRequest,
    productId :: Id Products,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Id DOrg.Organization,
    vehicleVariant :: DVeh.Variant,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
