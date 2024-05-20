{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareProduct (module Domain.Types.FareProduct, module ReExport) where

import Data.Aeson
import qualified Domain.Types.Common
import Domain.Types.Extra.FareProduct as ReExport
import qualified Domain.Types.Extra.FareProduct
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data FareProduct = FareProduct
  { area :: Lib.Types.SpecialLocation.Area,
    enabled :: Kernel.Prelude.Bool,
    farePolicyId :: Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy,
    id :: Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    searchSource :: Domain.Types.FareProduct.SearchSource,
    timeBounds :: Domain.Types.Extra.FareProduct.TimeBound,
    tripCategory :: Domain.Types.Common.TripCategory,
    vehicleServiceTier :: Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SearchSource = ALL | DASHBOARD | MOBILE_APP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchSource)
