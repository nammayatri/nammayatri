{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..)) where

import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DT
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as SQ

data IncentiveJourneyDimensions = IncentiveJourneyDimensions
  { merchantOperatingCityId :: Text,
    journeyId :: Maybe (Id DT.IncentiveJourney),
    enabled :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'IncentiveJourneyConfigDriver where
  type DimensionsFor 'IncentiveJourneyConfigDriver = IncentiveJourneyDimensions
  configTypeValue = IncentiveJourneyConfigDriver
  sConfigType = SIncentiveJourneyConfigDriver

instance ConfigDimensions IncentiveJourneyDimensions where
  type ConfigTypeOf IncentiveJourneyDimensions = 'IncentiveJourneyConfigDriver
  type ConfigValueTypeOf IncentiveJourneyDimensions = [DT.IncentiveJourney]
  getConfigType _ = IncentiveJourneyConfigDriver
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG IncentiveJourneyConfigDriver)
      (Id a.merchantOperatingCityId)
      (SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.journeyId) (Just . (.id)) (==),
        LCP.DimMatcher (.enabled) (Just . (.enabled)) (==)
      ]
      Nothing
  configFallback a =
    Just $ SQ.findByMerchantOperatingCityId (Id a.merchantOperatingCityId)
