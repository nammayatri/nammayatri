{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MetaWebhookConfig (module Domain.Types.MetaWebhookConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.MetaWebhookConfig as ReExport
import qualified Domain.Types.Extra.MetaWebhookConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MetaWebhookConfig = MetaWebhookConfig
  { accessToken :: Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text,
    apiVersion :: Kernel.Prelude.Text,
    appSecret :: Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text,
    baseUrl :: Kernel.Prelude.Text,
    botConfig :: Domain.Types.Extra.MetaWebhookConfig.MetaBotCfg,
    createdAt :: Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.MetaWebhookConfig.MetaWebhookConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    phoneNumberId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    verifyToken :: Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text
  }
  deriving (Generic, (Show), (ToJSON), (FromJSON))
