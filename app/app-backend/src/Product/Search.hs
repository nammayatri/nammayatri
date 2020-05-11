module Product.Search where

import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Ack
import Beckn.Utils.Common (withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.App

search :: Maybe RegToken -> SearchReq -> FlowHandler SearchRes
search regToken SearchReq {..} = withFlowHandler $ do
  let ack = Ack "search" "OK"
  return $ AckResponse context ack

search_cb :: Maybe RegToken -> OnSearchReq -> FlowHandler OnSearchRes
search_cb regToken OnSearchReq {..} = withFlowHandler $ do
  let ack = Ack "on_search" "OK"
  return $ AckResponse context ack
