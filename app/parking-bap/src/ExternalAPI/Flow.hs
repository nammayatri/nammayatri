module ExternalAPI.Flow where

import Beckn.Prelude
import Beckn.Types.App
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import qualified Core.API.Search as Search
import Core.API.Types (BecknReq)
import qualified Data.Text as T
import GHC.Records.Extra
import Tools.Metrics.Types (CoreMetrics)

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

callBecknAPIWithSignature ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    IsBecknAPI api req,
    HasInConfig r c "selfId" Text
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m ()
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.config.selfId)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d

getHttpManagerKey :: Text -> String
getHttpManagerKey keyId = signatureAuthManagerKey <> "-" <> T.unpack keyId
