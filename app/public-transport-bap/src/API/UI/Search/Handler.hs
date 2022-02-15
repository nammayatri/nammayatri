module API.UI.Search.Handler where

import API.UI.Search.Types as Search
import App.Types
import Beckn.Prelude
import Beckn.Utils.Common
import Product.Search as Search
import Tools.Auth

searchHandler :: PersonId -> Search.SearchReq -> FlowHandler Search.SearchRes
searchHandler personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  Search.searchHandler personId req

receiveFromKafka :: PersonId -> Search.SearchReq -> m ()
receiveFromKafka = error "not implemented"
