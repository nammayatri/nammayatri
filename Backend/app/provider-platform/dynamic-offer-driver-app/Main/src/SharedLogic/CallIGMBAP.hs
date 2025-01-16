{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallIGMBAP where

import qualified BecknV2.IGM.APIs as API
import qualified Data.HashMap.Strict as HMS
import qualified Domain.Types.Merchant as DM
import qualified EulerHS.Types as ET
import IGM.Enums as Spec
import IGM.Types as Spec
import qualified IssueManagement.Common as Common
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth
import Tools.Metrics (CoreMetrics)

callOnIssue ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c
  ) =>
  Spec.OnIssueReq ->
  BaseUrl ->
  Common.Merchant ->
  m ()
callOnIssue req bap_uri merchant = do
  let bppSubscriberId = getShortId $ merchant.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $
    withShortRetry $
      Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Spec.ON_ISSUE) API.onIssueAPI bap_uri internalEndPointHashMap req

callOnIssueStatus ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c
  ) =>
  Spec.OnIssueStatusReq ->
  BaseUrl ->
  DM.Merchant ->
  m ()
callOnIssueStatus req bap_uri merchant = do
  let bppSubscriberId = getShortId $ merchant.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $
    withShortRetry $
      Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Spec.ON_ISSUE_STATUS) API.onIssueStatusAPI bap_uri internalEndPointHashMap req
