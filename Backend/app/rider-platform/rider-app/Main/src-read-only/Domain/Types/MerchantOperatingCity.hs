{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantOperatingCity where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantOperatingCity = MerchantOperatingCity
  { city :: Kernel.Types.Beckn.Context.City,
    country :: Kernel.Types.Beckn.Context.Country,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverOfferMerchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    lat :: Kernel.Prelude.Double,
    long :: Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    state :: Kernel.Types.Beckn.Context.IndianState,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
