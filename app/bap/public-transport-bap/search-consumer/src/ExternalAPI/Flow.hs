module ExternalAPI.Flow where

import Core.Spec.API.Search as Search
import qualified Core.Spec.Search as Search
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Servant.SignatureAuth

search ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "bapId" r Text,
    HasField "gatewayUrl" r BaseUrl
  ) =>
  BecknReq Search.SearchMessage ->
  m ()
search req = do
  url <- asks (.gatewayUrl)
  callBecknAPIWithSignature "search" Search.searchAPI url req

callBecknAPIWithSignature ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    IsBecknAPI api req res,
    HasField "bapId" r Text
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapId)
  void $ callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
