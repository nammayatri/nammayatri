{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.UiDriverConfig where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data UiDriverConfig = UiDriverConfig
  { bundleVersion :: Kernel.Prelude.Maybe Data.Text.Text,
    config :: Data.Aeson.Value,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.UiDriverConfig.UiDriverConfig,
    language :: Kernel.External.Types.Language,
    os :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
