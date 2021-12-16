module API.Parking.Search.Types where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Core.Gps as Gps
import qualified Domain.Search as DSearch
import Servant
import Tools.Auth

type API =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes

data SearchReq = SearchReq
  { location :: Gps.Gps,
    fromDate :: UTCTime,
    toDate :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype SearchRes = SearchRes
  { searchId :: Id DSearch.Search
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
