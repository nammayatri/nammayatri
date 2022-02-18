module Domain.Endpoints.UI.Search where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.Spec.Common.Gps as BecknGps
import qualified Data.Text as T
import qualified Domain.Types.Search as DSearch
import qualified Storage.Queries.Search as QSearch
import Text.Read
import Tools.Auth

data SearchReq = SearchReq
  { location :: Gps,
    fromDate :: UTCTime,
    toDate :: UTCTime
  }
  deriving (Generic, FromJSON, ToSchema)

newtype SearchRes = SearchRes
  { searchId :: Id DSearch.Search
  }
  deriving (Generic, ToJSON, ToSchema)

data SearchMessage = SearchMessage
  { searchId :: Id DSearch.Search,
    gps :: BecknGps.Gps,
    toDate :: UTCTime
  }

data Gps = Gps
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, FromJSON, ToSchema)

search :: EsqDBFlow m r => PersonId -> SearchReq -> m (SearchRes, SearchMessage)
search personId req = do
  now <- getCurrentTime
  gps <- makeGps req.location
  searchRequest <- buildSearchRequest personId req now gps
  _ <- Esq.runTransaction $ QSearch.create searchRequest
  let searchMessage =
        SearchMessage
          { searchId = searchRequest.id,
            gps = gps,
            toDate = req.toDate
          }
  return (SearchRes searchRequest.id, searchMessage)

makeGps :: MonadFlow m => Gps -> m BecknGps.Gps
makeGps location = do
  lat <- readMaybe (T.unpack location.lat) & fromMaybeM (InternalError "Unable to parse lat")
  lon <- readMaybe (T.unpack location.lon) & fromMaybeM (InternalError "Unable to parse lon")
  pure BecknGps.Gps {..}

-- this is an exception when domain logic depends from Beckn type
buildSearchRequest ::
  MonadFlow m =>
  Id Person ->
  SearchReq ->
  UTCTime ->
  BecknGps.Gps ->
  m DSearch.Search
buildSearchRequest bapPersonId searchReq now gps = do
  id <- generateGUID
  return
    DSearch.Search
      { id = id,
        lat = gps.lat,
        lon = gps.lon,
        requestorId = bapPersonId,
        fromDate = searchReq.fromDate,
        toDate = searchReq.toDate,
        createdAt = now
      }
