{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Journey where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Journey = Journey
  { convenienceCost :: Kernel.Prelude.Int,
    estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    estimatedMaxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedMinFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    id :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legsDone :: Kernel.Prelude.Int,
    modes :: [Domain.Types.Common.TravelMode],
    searchRequestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    totalLegs :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
