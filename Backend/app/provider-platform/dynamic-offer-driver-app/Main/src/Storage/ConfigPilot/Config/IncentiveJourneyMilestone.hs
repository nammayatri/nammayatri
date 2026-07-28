{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.IncentiveJourneyMilestone
  ( IncentiveJourneyMilestoneDimensions (..),
    fetchAllMilestonesForCity,
  )
where

import qualified Domain.Types.IncentiveJourney as DIJ
import qualified Domain.Types.IncentiveJourneyMilestone as DT
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as SQMilestone

data IncentiveJourneyMilestoneDimensions = IncentiveJourneyMilestoneDimensions
  { merchantOperatingCityId :: Text,
    journeyId :: Maybe (Id DIJ.IncentiveJourney),
    milestoneId :: Maybe (Id DT.IncentiveJourneyMilestone)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'IncentiveJourneyMilestoneConfig where
  type DimensionsFor 'IncentiveJourneyMilestoneConfig = IncentiveJourneyMilestoneDimensions
  configTypeValue = IncentiveJourneyMilestoneConfig
  sConfigType = SIncentiveJourneyMilestoneConfig

instance ConfigDimensions IncentiveJourneyMilestoneDimensions where
  type ConfigTypeOf IncentiveJourneyMilestoneDimensions = 'IncentiveJourneyMilestoneConfig
  type ConfigValueTypeOf IncentiveJourneyMilestoneDimensions = [DT.IncentiveJourneyMilestone]
  getConfigType _ = IncentiveJourneyMilestoneConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG IncentiveJourneyMilestoneConfig)
      (Id a.merchantOperatingCityId)
      (fetchMilestones a)
      [ LCP.DimMatcher (.journeyId) (Just . (.journeyId)) (==),
        LCP.DimMatcher (.milestoneId) (Just . (.id)) (==)
      ]
      Nothing
  configFallback a = Just $ fetchMilestones a

fetchMilestones ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  IncentiveJourneyMilestoneDimensions ->
  m [DT.IncentiveJourneyMilestone]
fetchMilestones a =
  case a.journeyId of
    Just journeyId -> SQMilestone.findByJourneyId journeyId
    Nothing -> fetchAllMilestonesForCity (Id a.merchantOperatingCityId)

fetchAllMilestonesForCity ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DT.IncentiveJourneyMilestone]
fetchAllMilestonesForCity merchantOpCityId = do
  journeys <- CQJourney.findByMerchantOperatingCityId merchantOpCityId
  concat <$> mapM SQMilestone.findByJourneyId (map (.id) journeys)
