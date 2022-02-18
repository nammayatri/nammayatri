module API.UI.Search.Handler where

import App.Types
import Beckn.Prelude
import Beckn.Utils.Common
import qualified Core.ACL.Search as BecknACL
import Domain.Endpoints.UI.Search as DSearch
import qualified ExternalAPI.Flow as ExternalAPI
import Tools.Auth

searchHandler :: PersonId -> DSearch.SearchReq -> FlowHandler DSearch.SearchRes
searchHandler personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (searchRes, searchMessage) <- DSearch.search personId req
  becknSearchReq <- BecknACL.buildSearchReq searchMessage
  fork "search" . withRetry $ do
    -- do we need fork here?
    ExternalAPI.search becknSearchReq
  pure searchRes

receiveFromKafka :: PersonId -> DSearch.SearchReq -> m ()
receiveFromKafka = error "not implemented"
