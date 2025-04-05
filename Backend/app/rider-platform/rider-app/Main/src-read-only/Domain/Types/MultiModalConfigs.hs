{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalConfigs where

import Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Tools.Beam.UtilsTH

data MultiModalConfigs = MultiModalConfigs
  { busFilterTimeBufferInSeconds :: Kernel.Types.Time.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    enableBusFiltering :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.MultiModalConfigs.MultiModalConfigs,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    nearbyDriverSearchRadius :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
