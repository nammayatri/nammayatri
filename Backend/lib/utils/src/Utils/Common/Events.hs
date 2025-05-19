{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Common.Events where

-- import Network.HTTP.Types (status503)

-- import Data.List (lookup)

import qualified Control.Concurrent as TD
import Control.Concurrent.Async
import Data.UUID.V4 (nextRandom)
import qualified EulerHS.Runtime as R
import Kernel.Prelude hiding (app)
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog)
import Network.Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as NWI

timeoutEvent :: HasLog env => R.FlowRuntime -> env -> NWI.Response -> Int -> Middleware
timeoutEvent flowRt appEnv timeoutResponse seconds app req respond = do
  result <- race (app req respond) (TD.threadDelay (seconds * 1000000))
  case result of
    Left response -> pure response
    Right _ -> do
      requestId <- getRequestId $ Wai.requestHeaders req
      let path = Wai.rawPathInfo req
          query = Wai.rawQueryString req
      runFlowR flowRt appEnv $ timeoutLog requestId path query
      respond timeoutResponse
  where
    getRequestId headers = do
      let value = lookup "x-request-id" headers
      case value of
        Just val -> pure ("requestId-" <> decodeUtf8 val)
        Nothing -> pure "randomRequestId-" <> show <$> nextRandom
    timeoutLog logRequestId path query =
      logError $
        "Request timed out! "
          <> logRequestId
          <> " | Path: "
          <> decodeUtf8 path
          <> " | Query: "
          <> decodeUtf8 query
          <> " | Timeout: "
          <> show seconds
          <> " seconds"
