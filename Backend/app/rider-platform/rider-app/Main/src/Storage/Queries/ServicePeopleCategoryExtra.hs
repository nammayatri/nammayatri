{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.ServicePeopleCategoryExtra where

import Control.Applicative ((<|>))
import Data.Time hiding (getCurrentTime)
import Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.ServicePeopleCategory as BeamR
import Storage.Queries.OrphanInstances.ServicePeopleCategory ()

-- Extra code goes here --
findServicePeopleCategoryById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id ServicePeopleCategory -> Day -> m (Maybe ServicePeopleCategory)
findServicePeopleCategoryById id day = do
  servicePeopleCategories :: [ServicePeopleCategory] <- findAllWithKV [Se.Is BeamR.id $ Se.Eq $ getId id]
  let boundedServicePeopleCategories = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded) servicePeopleCategories) (UTCTime day $ secondsToDiffTime 19800)
  pure $ listToMaybe boundedServicePeopleCategories <|> find (\cfg -> cfg.timeBounds == Unbounded) servicePeopleCategories
