module Storage.Queries.ServicePeopleCategoryExtra where

import Control.Applicative ((<|>))
import Data.Time hiding (getCurrentTime)
import Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Sequelize as Se
import qualified Storage.Beam.ServicePeopleCategory as BeamR
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import Storage.Queries.OrphanInstances.ServicePeopleCategory ()

-- Extra code goes here --
findServicePeopleCategoryById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id ServicePeopleCategory -> Day -> m (Maybe ServicePeopleCategory)
findServicePeopleCategoryById id day = do
  servicePeopleCategories :: [ServicePeopleCategory] <- findAllWithKV [Se.Is BeamR.id $ Se.Eq $ getId id]
  mbRiderConfig <- case (.merchantOperatingCityId) =<< listToMaybe servicePeopleCategories of
    Nothing -> pure Nothing
    Just mocId -> getConfig (RiderConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  let currentLocalTime = UTCTime day $ secondsToDiffTime (fromIntegral timeDiffFromUtc.getSeconds)
  now <- getLocalCurrentTime timeDiffFromUtc
  if utctDay currentLocalTime == utctDay now
    then
      getServicePeopleCategory servicePeopleCategories currentLocalTime SameDay
        |<|>| getServicePeopleCategory servicePeopleCategories currentLocalTime AllDays
    else getServicePeopleCategory servicePeopleCategories currentLocalTime AllDays
  where
    getServicePeopleCategory servicePeopleCategories currentLocalTime pricingType = do
      let boundedServicePeopleCategories = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded && cfg.pricingType == pricingType) servicePeopleCategories) currentLocalTime
      pure $ listToMaybe boundedServicePeopleCategories <|> find (\cfg -> cfg.timeBounds == Unbounded && cfg.pricingType == pricingType) servicePeopleCategories

findByIdAndName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id ServicePeopleCategory] -> Text -> m (Maybe ServicePeopleCategory)
findByIdAndName id name = findOneWithKV [Se.And [Se.Is BeamR.id $ Se.In (map getId id), Se.Is BeamR.name $ Se.Eq name]]
