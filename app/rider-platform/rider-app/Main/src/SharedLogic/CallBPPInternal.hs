module SharedLogic.CallBPPInternal where

import Environment
import EulerHS.Types (EulerClient, client)
import Kernel.External.Encryption (DbHash)
import Kernel.External.Slack.Types
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common hiding (Error)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Text,
    customerNumberHash :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type LinkRefereeAPI =
  "internal"
    :> Capture "merchantId" Text
    :> "referee"
    :> Header "token" Text
    :> ReqBody '[JSON] RefereeLinkInfoReq
    :> Post '[JSON] APISuccess

linkRefereeClient :: Text -> Maybe Text -> RefereeLinkInfoReq -> EulerClient APISuccess
linkRefereeClient = client likeRefereeApi
  where
    likeRefereeApi :: Proxy LinkRefereeAPI
    likeRefereeApi = Proxy

linkReferee ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  DbHash ->
  m APISuccess
linkReferee apiKey internalUrl merchantId referralCode customerPhNumHash = do
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") internalUrl (linkRefereeClient merchantId (Just apiKey) (RefereeLinkInfoReq referralCode (decodeUtf8 customerPhNumHash.unDbHash))) "LinkReferee"
