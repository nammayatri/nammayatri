{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.SearchRequest where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.Location
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequestForDriver
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data SearchRequestOfDriver = SearchRequestOfDriver
  { createdAt :: Kernel.Prelude.UTCTime,
    fromLocation :: Domain.Types.Location.Location,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tripEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    tripEstimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchRequestsReq = SearchRequestsReq
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fromDate :: Kernel.Prelude.UTCTime,
    toDate :: Kernel.Prelude.UTCTime,
    mbLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    mbOffset :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    mbOnlyActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SearchRequestsReq where
  hideSecrets = Kernel.Prelude.identity

data SearchRequestsRes = SearchRequestsRes {searchrequests :: [SearchRequestOfDriver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("searchRequest" :> PostSearchRequestSearchrequests)

type PostSearchRequestSearchrequests = ("searchrequests" :> ReqBody ('[JSON]) SearchRequestsReq :> Post ('[JSON]) SearchRequestsRes)

newtype SearchRequestAPIs = SearchRequestAPIs {postSearchRequestSearchrequests :: (SearchRequestsReq -> EulerHS.Types.EulerClient SearchRequestsRes)}

mkSearchRequestAPIs :: (Client EulerHS.Types.EulerClient API -> SearchRequestAPIs)
mkSearchRequestAPIs searchRequestClient = (SearchRequestAPIs {..})
  where
    postSearchRequestSearchrequests = searchRequestClient

data SearchRequestUserActionType
  = POST_SEARCH_REQUEST_SEARCHREQUESTS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SearchRequestUserActionType where
  toJSON (POST_SEARCH_REQUEST_SEARCHREQUESTS) = Data.Aeson.String "POST_SEARCH_REQUEST_SEARCHREQUESTS"

instance FromJSON SearchRequestUserActionType where
  parseJSON (Data.Aeson.String "POST_SEARCH_REQUEST_SEARCHREQUESTS") = pure POST_SEARCH_REQUEST_SEARCHREQUESTS
  parseJSON _ = fail "POST_SEARCH_REQUEST_SEARCHREQUESTS expected"

$(Data.Singletons.TH.genSingletons [(''SearchRequestUserActionType)])
