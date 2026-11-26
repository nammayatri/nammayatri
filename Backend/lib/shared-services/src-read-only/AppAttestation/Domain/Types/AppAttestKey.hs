{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AppAttestation.Domain.Types.AppAttestKey where

import qualified AppAttestation.Domain.Types.Common
import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AppAttestKey = AppAttestKey
  { bundleId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    environment :: AppAttestation.Domain.Types.Common.AppAttestEnvironment,
    keyId :: Kernel.Prelude.Text,
    lastUsedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id AppAttestation.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id AppAttestation.Domain.Types.Common.MerchantOperatingCity,
    publicKey :: Kernel.Prelude.Text,
    receipt :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    signCount :: Kernel.Prelude.Int,
    status :: AppAttestation.Domain.Types.Common.AppAttestKeyStatus,
    teamId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    useCount :: Kernel.Prelude.Int
  }
  deriving (Generic, Show)
