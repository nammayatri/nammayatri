{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AssetRelease (module Domain.Types.AssetRelease, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.AssetRelease as ReExport
import qualified Domain.Types.Extra.AssetRelease
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AssetRelease = AssetRelease
  { assetType :: Domain.Types.Extra.AssetRelease.AssetType,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.AssetRelease.AssetRelease,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rolledBackAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    sha256 :: Kernel.Prelude.Text,
    sizeBytes :: Kernel.Prelude.Int,
    sourceRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    url :: Kernel.Prelude.Text,
    version :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
