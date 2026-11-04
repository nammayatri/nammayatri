{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSFleetStats where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSFleetStats = FRFSFleetStats
  { createdAt :: Kernel.Prelude.UTCTime,
    fleetNumber :: Data.Text.Text,
    gtfsId :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSFleetStats.FRFSFleetStats,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    rating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    totalRatingCount :: Kernel.Prelude.Int,
    totalRatingScore :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
