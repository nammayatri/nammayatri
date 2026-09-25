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

instance ConfigTypeInfo 'IncentiveJourneyConfigRider where
  type DimensionsFor 'IncentiveJourneyConfigRider = IncentiveJourneyDimensions
  configTypeValue = IncentiveJourneyConfigRider
  sConfigType = SIncentiveJourneyConfigRider

instance ConfigDimensions IncentiveJourneyDimensions where
  type ConfigTypeOf IncentiveJourneyDimensions = 'IncentiveJourneyConfigRider
  type ConfigValueTypeOf IncentiveJourneyDimensions = [DT.IncentiveJourney]
  getConfigType _ = IncentiveJourneyConfigRider
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG IncentiveJourneyConfigRider)
      (Id a.merchantOperatingCityId)
      (SQ.findByMerchantOperatingCityId IJ.RiderActor (Id a.merchantOperatingCityId))
      [ LCP.DimMatcher (.merchantId) (Just . (.merchantId.getId)) (==),
        LCP.DimMatcher (.journeyId) (Just . (.id)) (==),
        LCP.DimMatcher (.enabled) (Just . (.enabled)) (==),
        LCP.DimMatcher (.journeyType) (Just . IJ.journeyTypeOrDefault . (.journeyType)) (==)
      ]
      Nothing
  configFallback a =
    Just $ SQ.findByMerchantOperatingCityId IJ.RiderActor (Id a.merchantOperatingCityId)
