{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module BecknV2.OnDemand.Utils.Callback where

-- import Control.Lens.Operators ((?~))

-- import Kernel.Types.Id
-- import Kernel.Utils.Servant.SignatureAuth

-- import qualified Kernel.Types.Beckn.Context as M.Context
-- import Kernel.Types.Beckn.ReqTypes

-- import Kernel.Types.Error.BaseError.HTTPError.BecknAPIError
-- import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))

import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Context (mapToCbAction)
import Control.Lens ((.~))
import qualified Data.HashMap as HM
import EulerHS.Prelude hiding ((.~))
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Beckn.Ack
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common hiding (someExceptionToBecknApiError)
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant.Client

-- someExceptionToBecknApiError :: SomeException -> Spec.Error
-- someExceptionToBecknApiError exc
--   | Just (HTTPException err) <- fromException exc = Spec.Error err.toErrorCode err.toMessage Nothing
--   | otherwise = Spec.Error "E_500" (show exc) Nothing

-- someExceptionToCallbackReqMig :: Spec.Context -> SomeException -> Spec.OnSearchReq
-- someExceptionToCallbackReqMig onSearchReqContext exc =
--   let err = someExceptionToBecknApiError exc
--    in Spec.OnSearchReq
--         { onSearchReqError = Just err,
--           onSearchReqContext,
--           onSearchReqMessage = Nothing
--         }

type WithBecknCallbackMig api req m =
  ( MonadFlow m,
    SanitizedUrl api,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (Spec.OnSearchReq -> ET.EulerClient AckResponse)
  ) =>
  Text ->
  Proxy api ->
  Spec.Context ->
  BaseUrl ->
  HM.Map BaseUrl BaseUrl ->
  Spec.OnSearchReq ->
  m AckResponse

withBecknCallbackMig ::
  (m () -> m ()) ->
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api req m
withBecknCallbackMig doWithCallback auth actionName api context cbUrl internalEndPointHashMap req = do
  now <- getCurrentTime
  cbAction <-
    mapToCbAction actionName
      & fromMaybeM (InternalError $ "Beckn " <> show actionName <> " action doesn't have callback")
  let cbContext =
        context
          & #contextAction .~ (Just cbAction)
          & #contextTimestamp .~ (Just now)
  forkBecknCallback
    (req)
    (cbContext)
    (doWithCallback . void . callBecknAPI auth Nothing (show cbAction) api cbUrl internalEndPointHashMap)
    (show actionName)
  return Ack

-- (Spec.OnSearchReq cbContext)

forkBecknCallback ::
  (Forkable m, MonadCatch m, Log m) =>
  Spec.OnSearchReq ->
  Spec.Context ->
  -- (error -> result) ->
  -- (success -> result) ->
  (Spec.OnSearchReq -> m ()) ->
  Text ->
  m ()
forkBecknCallback req cbContext doWithResult actionName =
  fork actionName $
    case req.onSearchReqError of
      Nothing -> do
        doWithResult $ Spec.OnSearchReq cbContext Nothing req.onSearchReqMessage
      Just err -> do
        logError $ "Error executing callback action " <> actionName <> ": " <> show err
        doWithResult $ Spec.OnSearchReq cbContext req.onSearchReqError Nothing
