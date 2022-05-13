module Product.RentalSearch where

import App.Types
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Core.ACL.Search as ACL
import qualified Domain.Action.UI.Search.Rental as DRentalSearch
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Types.API.RentalSearch as API
import Utils.Common

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (searchRes, dSearchReq) <- DRentalSearch.search personId req
  fork "search rental" . withRetry $ do
    -- do we need fork here?
    becknReq <- ACL.buildRentalSearchReq dSearchReq
    void $ ExternalAPI.search becknReq
  pure searchRes
