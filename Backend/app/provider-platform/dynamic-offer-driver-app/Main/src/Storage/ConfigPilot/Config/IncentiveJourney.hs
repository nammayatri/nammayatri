{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..)) where

import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.IncentiveJourney as DT
import Domain.Types.VehicleCategory as DTV
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as SQ

data IncentiveJourneyDimensions = IncentiveJourneyDimensions
  { merchantOperatingCityId :: Text,
    journeyId :: Maybe (Id DT.IncentiveJourney),
    enabled :: Maybe Bool,
    vehicleCategory :: Maybe DTV.VehicleCategory,
    serviceTierType :: Maybe DCommon.ServiceTierType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'IncentiveJourneyConfig where
  type DimensionsFor 'IncentiveJourneyConfig = IncentiveJourneyDimensions
  configTypeValue = IncentiveJourneyConfig
  sConfigType = SIncentiveJourneyConfig

instance ConfigDimensions IncentiveJourneyDimensions where
  type ConfigTypeOf IncentiveJourneyDimensions = 'IncentiveJourneyConfig
  type ConfigValueTypeOf IncentiveJourneyDimensions = [DT.IncentiveJourney]
  getConfigType _ = IncentiveJourneyConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG IncentiveJourneyConfig)
      (Id a.merchantOperatingCityId)
      (SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.journeyId) (Just . (.id)) (==),
        LCP.DimMatcher (.enabled) (Just . (.enabled)) (==),
        LCP.DimMatcher (.vehicleCategory) (.vehicleCategory) (==),
        LCP.DimMatcher (.serviceTierType) (.serviceTierType) (==)
      ]
      Nothing
  configFallback a =
    Just $ SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId)
