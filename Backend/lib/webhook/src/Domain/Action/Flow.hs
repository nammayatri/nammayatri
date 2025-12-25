module Domain.Action.Flow where

import qualified Data.Text.Encoding as DT
import qualified Domain.Types.WebhookExtra as Webhook
import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import Servant hiding (throwError)

mkBasicAuthData :: Text -> Text -> BasicAuthData
mkBasicAuthData userName password =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 userName,
      basicAuthPassword = DT.encodeUtf8 password
    }

type NYWebhookAPI =
  "Service"
    :> "Yatri"
    :> "Webhook"
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[JSON] Webhook.ExternalWebhookData
    :> Post '[JSON] APISuccess

nyWebhook ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  EncryptedField 'AsEncrypted Text ->
  Text ->
  Webhook.ExternalWebhookData ->
  m APISuccess
nyWebhook url password userName req = do
  passwordDecrypted <- decrypt password
  let proxy = Proxy @NYWebhookAPI
      eulerClient = Euler.client proxy (mkBasicAuthData userName passwordDecrypted) req
  callAPI url eulerClient "ny-ext-webhook" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> "ny-ext-webhook" <> " API: " <> show err)
