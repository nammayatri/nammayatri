module Domain.Types.SearchRetry where

import qualified Beckn.Types.Core.Taxi.Select as Select
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import Kernel.Types.Id

data SearchRetry = SearchRetry
  { id :: Id SearchRequest,
    parentSearchId :: Id SearchRequest,
    retryCreatedAt :: UTCTime,
    retryType :: Select.RetryType
  }
  deriving (Generic, Show)
