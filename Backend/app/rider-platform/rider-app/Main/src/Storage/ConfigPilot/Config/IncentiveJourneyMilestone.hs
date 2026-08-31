{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.IncentiveJourneyMilestone
  ( IncentiveJourneyMilestoneDimensions (..),
    fetchAllMilestonesForCity,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DT
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IncentiveJourney as CQJourney
import qualified Storage.CachedQueries.IncentiveJourneyMilestone as SQMilestone

data IncentiveJourneyMilestoneDimensions = IncentiveJourneyMilestoneDimensions
  { merchantOperatingCityId :: Text,
    journeyId :: Maybe (Id DIJ.IncentiveJourney),
    milestoneId :: Maybe (Id DT.IncentiveJourneyMilestone)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'IncentiveJourneyMilestoneConfigRider where
  type DimensionsFor 'IncentiveJourneyMilestoneConfigRider = IncentiveJourneyMilestoneDimensions
  configTypeValue = IncentiveJourneyMilestoneConfigRider
  sConfigType = SIncentiveJourneyMilestoneConfigRider

instance ConfigDimensions IncentiveJourneyMilestoneDimensions where
  type ConfigTypeOf IncentiveJourneyMilestoneDimensions = 'IncentiveJourneyMilestoneConfigRider
  type ConfigValueTypeOf IncentiveJourneyMilestoneDimensions = [DT.IncentiveJourneyMilestone]
  getConfigType _ = IncentiveJourneyMilestoneConfigRider
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.RIDER_CONFIG IncentiveJourneyMilestoneConfigRider)
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
  concat <$> mapM (SQMilestone.findByJourneyId . (.id)) journeys
