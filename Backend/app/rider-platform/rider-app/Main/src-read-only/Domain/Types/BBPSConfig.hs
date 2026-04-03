{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BBPSConfig where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Encryption
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Tools.Beam.UtilsTH



data BBPSConfig
    = BBPSConfig {bbpsAgentId :: Kernel.Prelude.Text,
                  bbpsServerUrl :: Kernel.Prelude.Text,
                  bbpsSignatureKey :: Kernel.External.Encryption.EncryptedField 'Kernel.External.Encryption.AsEncrypted Kernel.Prelude.Text,
                  merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                  createdAt :: Kernel.Prelude.UTCTime,
                  updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic



