module API.Utils where

import Environment
import Kernel.Mock.App
import Kernel.Types.Beckn.Context
import Kernel.Types.Time
import Kernel.Types.TimeRFC339
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
