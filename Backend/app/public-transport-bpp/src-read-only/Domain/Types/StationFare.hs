{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.StationFare where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data StationFare = StationFare
  { currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    fareAmount :: Kernel.Types.Common.HighPrecMoney,
    fromStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    id :: Kernel.Types.Id.Id Domain.Types.StationFare.StationFare,
    toStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
