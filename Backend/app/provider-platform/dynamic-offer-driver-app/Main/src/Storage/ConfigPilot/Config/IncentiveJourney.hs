{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.IncentiveJourney (IncentiveJourneyDimensions (..)) where

import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.IncentiveJourney as IJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DT
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourney as SQ
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Yudhishthira ()

data IncentiveJourneyDimensions = IncentiveJourneyDimensions
  { merchantOperatingCityId :: Text,
    merchantId :: Maybe Text,
    journeyId :: Maybe (Id DT.IncentiveJourney),
    enabled :: Maybe Bool,
    journeyType :: Maybe DT.IncentiveJourneyType
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
      (SQ.findByMerchantOperatingCityId IJ.DriverActor (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.merchantId) (Just . (.merchantId.getId)) (==),
        LCP.DimMatcher (.journeyId) (Just . (.id)) (==),
        LCP.DimMatcher (.enabled) (Just . (.enabled)) (==),
        LCP.DimMatcher (.journeyType) (Just . IJ.journeyTypeOrDefault . (.journeyType)) (==)
      ]
      Nothing
  configFallback a =
    Just $ SQ.findByMerchantOperatingCityId IJ.DriverActor (Id a.merchantOperatingCityId)
