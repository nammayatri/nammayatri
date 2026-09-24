{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Search where

import qualified "this" API.UI.Search
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("search" :> PostSearchRide)

type PostSearchRide = (Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "rideSearch" :> ReqBody '[JSON] API.UI.Search.SearchReq :> Post '[JSON] API.UI.Search.SearchResp)

newtype SearchAPIs = SearchAPIs {postSearchRide :: Kernel.Types.Id.Id Domain.Types.Person.Person -> API.UI.Search.SearchReq -> EulerHS.Types.EulerClient API.UI.Search.SearchResp}

mkSearchAPIs :: (Client EulerHS.Types.EulerClient API -> SearchAPIs)
mkSearchAPIs searchClient = (SearchAPIs {..})
  where
    postSearchRide = searchClient

data SearchUserActionType
  = POST_SEARCH_RIDE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SearchUserActionType where
  toJSON POST_SEARCH_RIDE = Data.Aeson.String "POST_SEARCH_RIDE"

instance FromJSON SearchUserActionType where
  parseJSON (Data.Aeson.String "POST_SEARCH_RIDE") = pure POST_SEARCH_RIDE
  parseJSON _ = fail "POST_SEARCH_RIDE expected"

$(Data.Singletons.TH.genSingletons [''SearchUserActionType])
