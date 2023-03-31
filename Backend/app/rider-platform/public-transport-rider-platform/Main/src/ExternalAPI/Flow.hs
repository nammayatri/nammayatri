{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ExternalAPI.Flow where

import qualified Beckn.Spec.API.Confirm as Confirm
import qualified Beckn.Spec.API.Status as Status
import Beckn.Spec.Confirm
import qualified Beckn.Spec.Status as Status
import qualified EulerHS.Types as ET
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth

confirm ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text
  ) =>
  BaseUrl ->
  BecknReq ConfirmMessage ->
  m ()
confirm bppUrl req = do
  callBecknAPIWithSignature "confirm" Confirm.confirmAPI bppUrl req

status ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text
  ) =>
  BaseUrl ->
  BecknReq Status.StatusMessage ->
  m ()
status = callBecknAPIWithSignature "status" Status.statusAPI

callBecknAPIWithSignature ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    SanitizedUrl api,
    IsBecknAPI api req res,
    HasField "selfId" r Text
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.selfId)
  void $ callBecknAPI (Just $ ET.ManagerSelector $ getHttpManagerKey bapId) Nothing a b c d
