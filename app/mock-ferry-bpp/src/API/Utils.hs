module API.Utils where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Context
import Types.App

buildOnActionContext :: Action -> Context -> MockM Context
buildOnActionContext action ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = action,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
