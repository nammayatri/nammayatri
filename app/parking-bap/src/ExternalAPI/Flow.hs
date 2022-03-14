module ExternalAPI.Flow where

import Beckn.Prelude
import Beckn.Types.App
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import qualified Core.API.Confirm as Confirm
import qualified Core.API.Search as Search
import qualified Core.API.Status as Status
import qualified Core.Confirm as Confirm
import qualified Core.Search as Search
import qualified Core.Status as Status
import qualified Data.Text as T
import GHC.Records.Extra
import Tools.Metrics (CoreMetrics)

search ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasInConfig r c "selfId" Text,
    HasInConfig r c "gatewayUrl" BaseUrl
  ) =>
  BecknReq Search.SearchIntent ->
  m ()
search req = do
  url <- asks (.config.gatewayUrl)
  callBecknAPIWithSignature "search" Search.searchAPI url req

confirm ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasInConfig r c "selfId" Text
  ) =>
  BaseUrl ->
  BecknReq Confirm.ConfirmMessage ->
  m ()
confirm = callBecknAPIWithSignature "сonfirm" Confirm.confirmAPI

triggerStatusUpdate ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasInConfig r c "selfId" Text
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
    HasInConfig r c "selfId" Text
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.config.selfId)
  void $ callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d

getHttpManagerKey :: Text -> String
getHttpManagerKey keyId = signatureAuthManagerKey <> "-" <> T.unpack keyId
