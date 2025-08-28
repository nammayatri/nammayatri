module Storage.Queries.Transformers.PartnerOrgConfig where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Domain.Types.PartnerOrgConfig as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (MonadFlow, throwError)

getTypeAndJSONFromPOrgConfig :: PartnerOrganizationConfig -> (ConfigType, A.Value)
getTypeAndJSONFromPOrgConfig pOrgCfg =
  let cfgJson = Domain.getConfigJSON pOrgCfg
      cfgType = Domain.getConfigType pOrgCfg
   in (cfgType, cfgJson)

getPOrgConfigFromTypeAndJson :: MonadFlow m => A.Value -> ConfigType -> m PartnerOrganizationConfig
getPOrgConfigFromTypeAndJson config = \case
  REGISTRATION -> Registration <$> parseCfgJSON config REGISTRATION
  RATE_LIMIT -> RateLimit <$> parseCfgJSON config RATE_LIMIT
  TICKET_SMS -> TicketSMS <$> parseCfgJSON config TICKET_SMS
  BPP_STATUS_CALL -> BPPStatusCall <$> parseCfgJSON config BPP_STATUS_CALL
  WALLET_CLASS_NAME -> WalletClassName <$> parseCfgJSON config WALLET_CLASS_NAME
  WALLET_QR_TYPE -> WalletQRType <$> parseCfgJSON config WALLET_QR_TYPE
  where
    parseCfgJSON cfg cfgType = do
      case A.fromJSON cfg of
        A.Success a -> pure a
        A.Error msg -> throwError . InternalError $ "Failed to parse PartnerOrgConfig JSON for cfgType: " <> show cfgType <> ", value: " <> show cfg <> ", error: " <> T.pack msg
