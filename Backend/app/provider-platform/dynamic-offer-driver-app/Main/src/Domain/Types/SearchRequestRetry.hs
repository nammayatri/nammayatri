module Domain.Types.SearchRequestRetry where

import qualified Beckn.Types.Core.Taxi.Select as Select
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import Kernel.Types.Id

data SearchRequestRetry = SearchRequestRetry
  { id :: Id SearchRequestRetry,
    parentSearchId :: Id SearchRequest,
    retryCreatedAt :: UTCTime,
    retryType :: Select.RetryType
  }
  deriving (Generic, Show)
