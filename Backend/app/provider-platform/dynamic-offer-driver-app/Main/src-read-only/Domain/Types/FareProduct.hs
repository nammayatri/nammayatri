{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareProduct where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data FareProduct = FareProduct
  { area :: Lib.Types.SpecialLocation.Area,
    disableRecompute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    disableTollRecompute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    enabled :: Kernel.Prelude.Bool,
    farePolicyId :: Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy,
    id :: Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    searchSource :: Domain.Types.FareProduct.SearchSource,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    tripCategory :: Domain.Types.Common.TripCategory,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SearchSource = ALL | DASHBOARD | MOBILE_APP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchSource)
