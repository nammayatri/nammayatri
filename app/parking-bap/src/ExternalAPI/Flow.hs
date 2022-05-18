module ExternalAPI.Flow where

import Beckn.Prelude
import Beckn.Types.App
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.API.Confirm as Confirm
import qualified Core.API.Search as Search
import qualified Core.API.Status as Status
import qualified Core.Confirm as Confirm
import qualified Core.Search as Search
import qualified Core.Status as Status
import GHC.Records.Extra
import Tools.Metrics (CoreMetrics)

search ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text,
    HasField "gatewayUrl" r BaseUrl
  ) =>
  BecknReq Search.SearchIntent ->
  m ()
search req = do
  url <- asks (.gatewayUrl)
  callBecknAPIWithSignature "search" Search.searchAPI url req

confirm ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text
  ) =>
  BaseUrl ->
  BecknReq Confirm.ConfirmMessage ->
  m ()
confirm = callBecknAPIWithSignature "сonfirm" Confirm.confirmAPI

triggerStatusUpdate ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "selfId" r Text
  ) =>
  BaseUrl ->
  BecknReq Status.StatusMessage ->
  m ()
triggerStatusUpdate = callBecknAPIWithSignature "status" Status.statusAPI

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
