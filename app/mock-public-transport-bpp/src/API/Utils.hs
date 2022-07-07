module API.Utils where

import Beckn.Mock.App
import Beckn.Types.Core.Context
import Beckn.Types.Time
import Beckn.Types.TimeRFC339
import Environment
import Relude

buildOnActionContext :: Action -> Context -> MockM AppEnv Context
buildOnActionContext action ctx = do
  now <- UTCTimeRFC3339 <$> getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = action,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
