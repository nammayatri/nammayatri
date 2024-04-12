{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallFRFSBPP where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Data.HashMap.Strict as HM
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth
import Tools.Metrics (CoreMetrics)

search ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "requestId" r (Maybe Text)
  ) =>
  BaseUrl ->
  Spec.SearchReq ->
  m Spec.AckResponse
search gatewayUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  mbRequestId <- asks (.requestId)
  bapId <- req.searchReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature mbRequestId bapId "search" Spec.searchAPI gatewayUrl internalEndPointHashMap req

init ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "requestId" r (Maybe Text)
  ) =>
  BaseUrl ->
  Spec.InitReq ->
  m Spec.AckResponse
init providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  mbRequestId <- asks (.requestId)
  bapId <- req.initReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature mbRequestId bapId "init" Spec.initAPI providerUrl internalEndPointHashMap req

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "requestId" r (Maybe Text)
  ) =>
  BaseUrl ->
  Spec.ConfirmReq ->
  m Spec.AckResponse
confirm providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  mbRequestId <- asks (.requestId)
  bapId <- req.confirmReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature mbRequestId bapId "confirm" Spec.confirmAPI providerUrl internalEndPointHashMap req

status ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "requestId" r (Maybe Text)
  ) =>
  BaseUrl ->
  Spec.StatusReq ->
  m Spec.AckResponse
status providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  mbRequestId <- asks (.requestId)
  bapId <- req.statusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature mbRequestId bapId "status" Spec.statusAPI providerUrl internalEndPointHashMap req

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "requestId" r (Maybe Text)
  ) =>
  BaseUrl ->
  Spec.CancelReq ->
  m Spec.AckResponse
cancel providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  mbRequestId <- asks (.requestId)
  bapId <- req.cancelReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature mbRequestId bapId "cancel" Spec.cancelAPI providerUrl internalEndPointHashMap req

callBecknAPIWithSignature ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api
  ) =>
  Maybe Text ->
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature mbRequestId a = callBecknAPI mbRequestId (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing
