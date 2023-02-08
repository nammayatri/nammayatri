module ExternalAPI.Flow where

import qualified Core.Spec.API.Confirm as Confirm
import qualified Core.Spec.API.Status as Status
import Core.Spec.Confirm
import qualified Core.Spec.Status as Status
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
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
  void $ callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
