{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.UiDriverConfig where

import Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data UiDriverConfig = UiDriverConfig
  { config :: Data.Aeson.Value,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.UiDriverConfig.UiDriverConfig,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    os :: Kernel.Types.Version.DeviceType,
    platform :: Lib.Yudhishthira.Types.PlatformType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
