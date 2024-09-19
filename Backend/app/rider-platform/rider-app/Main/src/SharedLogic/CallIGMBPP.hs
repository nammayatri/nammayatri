{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallIGMBPP where

import qualified BecknV2.IGM.APIs as Spec
import qualified Data.HashMap.Strict as HM
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import qualified IGM.Types as Spec
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth
import Tools.Metrics (CoreMetrics)

issue ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  Spec.IssueReq ->
  m Spec.AckResponse
issue providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature bapId "issue" Spec.issueAPI providerUrl internalEndPointHashMap req

onIssue ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  Spec.OnIssueReq ->
  m Spec.AckResponse
onIssue providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bppId <- req.onIssueReqContext.contextBppId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature bppId "onIssue" Spec.onIssueAPI providerUrl internalEndPointHashMap req

issueStatus ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  Spec.IssueStatusReq ->
  m Spec.AckResponse
issueStatus providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.issueStatusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature bapId "issueStatus" Spec.issueStatusAPI providerUrl internalEndPointHashMap req

callBecknAPIWithSignature ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api
  ) =>
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature a = callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing
