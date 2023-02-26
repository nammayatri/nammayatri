module SharedLogic.CallBPPInternal where

import Environment
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Text,
    customerNumberHash :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type LinkRefereeAPI =
  "referee"
    :> SignatureAuth "Authorization"
    :> ReqBody '[JSON] RefereeLinkInfoReq
    :> Post '[JSON] APISuccess

linkRefereeAPI :: Proxy LinkRefereeAPI
linkRefereeAPI = Proxy

linkReferee ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  Text ->
  DbHash ->
  m APISuccess
linkReferee internalUrl referralCode customerPhNumHash = do
  callBecknAPIWithSignature "linkReferee" linkRefereeAPI internalUrl (RefereeLinkInfoReq referralCode (decodeUtf8 customerPhNumHash.unDbHash))

callBecknAPIWithSignature,
  callBecknAPIWithSignatureMetro ::
    ( MonadFlow m,
      CoreMetrics m,
      IsBecknAPI api req res,
      HasBapInfo r m
    ) =>
    Text ->
    Proxy api ->
    BaseUrl ->
    req ->
    m res
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapSelfIds.cabs)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
callBecknAPIWithSignatureMetro a b c d = do
  bapId <- asks (.bapSelfIds.metro)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
