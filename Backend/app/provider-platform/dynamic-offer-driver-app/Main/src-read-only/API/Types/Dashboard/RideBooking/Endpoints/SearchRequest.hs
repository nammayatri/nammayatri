{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.RideBooking.Endpoints.SearchRequest where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.Location
import qualified Kernel.Types.Id
import qualified Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchRequest
import qualified Kernel.Types.Common
import qualified "this" Domain.Types.Person
import qualified EulerHS.Types
import qualified Kernel.Types.HideSecrets
import qualified Data.Singletons.TH



data SearchReqInfoRes
    = SearchReqInfoRes {acceptedCount :: Kernel.Prelude.Int, emptyCount :: Kernel.Prelude.Int, pulledCount :: Kernel.Prelude.Int, rejectedCount :: Kernel.Prelude.Int, totalCount :: Kernel.Prelude.Int}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data SearchRequestOfDriver
    = SearchRequestOfDriver {createdAt :: Kernel.Prelude.UTCTime,
                             fromLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
                             id :: Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver,
                             requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
                             toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
                             tripEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
                             tripEstimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data SearchRequestsReq
    = SearchRequestsReq {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                         fromDate :: Kernel.Prelude.UTCTime,
                         toDate :: Kernel.Prelude.UTCTime,
                         mbLimit :: Kernel.Prelude.Int,
                         mbOffset :: Kernel.Prelude.Int,
                         mbOnlyActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets SearchRequestsReq
    where hideSecrets = Kernel.Prelude.identity
data SearchRequestsRes
    = SearchRequestsRes {searchrequests :: [SearchRequestOfDriver]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("searchRequest" :> (PostSearchRequestSearchrequests :<|> GetSearchRequestList :<|> GetSearchRequestInfo))
type PostSearchRequestSearchrequests = ("searchrequests" :> ReqBody ('[JSON]) SearchRequestsReq :> Post ('[JSON]) SearchRequestsRes)
type GetSearchRequestList = ("list" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> MandatoryQueryParam "fromDate"
                                                                                                                                             Kernel.Prelude.UTCTime :> MandatoryQueryParam "toDate" Kernel.Prelude.UTCTime :> MandatoryQueryParam "limit" Kernel.Prelude.Int :> MandatoryQueryParam "offset"
                                                                                                                                                                                                                                                                                                    Kernel.Prelude.Int :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                              SearchRequestsRes)
type GetSearchRequestInfo = ("info" :> MandatoryQueryParam "fromDate" Kernel.Prelude.UTCTime :> MandatoryQueryParam "toDate" Kernel.Prelude.UTCTime :> MandatoryQueryParam "driverId"
                                                                                                                                                                           (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get ('[JSON]) SearchReqInfoRes)
data SearchRequestAPIs
    = SearchRequestAPIs {postSearchRequestSearchrequests :: (SearchRequestsReq -> EulerHS.Types.EulerClient SearchRequestsRes),
                         getSearchRequestList :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient SearchRequestsRes),
                         getSearchRequestInfo :: (Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient SearchReqInfoRes)}
mkSearchRequestAPIs :: (Client EulerHS.Types.EulerClient API -> SearchRequestAPIs)
mkSearchRequestAPIs searchRequestClient = (SearchRequestAPIs {..})
                        where postSearchRequestSearchrequests :<|> getSearchRequestList :<|> getSearchRequestInfo = searchRequestClient
data SearchRequestUserActionType
    = POST_SEARCH_REQUEST_SEARCHREQUESTS | GET_SEARCH_REQUEST_LIST | GET_SEARCH_REQUEST_INFO
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''SearchRequestUserActionType)])

