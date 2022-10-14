{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy.FareProduct
  ( findEnabledByOrgId,
    findEnabledByOrgIdAndType,
    insertIfNotExist,
    delete,
    clearCache,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.FareProduct
import Domain.Types.Organization (Organization)
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy.FareProduct as Queries

findEnabledByOrgId ::
  (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) =>
  Id Organization ->
  m [FareProduct]
findEnabledByOrgId id =
  Hedis.get (makeAllOrgIdKey id) >>= \case
    Just a -> return $ coerce @(FareProductD 'Unsafe) @FareProduct <$> a
    Nothing -> cacheRes /=<< Queries.findEnabledByOrgId id
  where
    cacheRes fareProds = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllOrgIdKey id) (coerce @[FareProduct] @[FareProductD 'Unsafe] fareProds) expTime

findEnabledByOrgIdAndType ::
  (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) =>
  Maybe FareProductType ->
  Id Organization ->
  m [FareProduct]
findEnabledByOrgIdAndType mbFPType orgId =
  Hedis.get (makeAllOrgIdTypeKey orgId mbFPType) >>= \case
    Just a -> return $ fmap (coerce @(FareProductD 'Unsafe) @FareProduct) a
    Nothing -> cacheRes /=<< Queries.findEnabledByOrgIdAndType mbFPType orgId
  where
    cacheRes fareProds = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllOrgIdTypeKey orgId mbFPType) (coerce @[FareProduct] @[FareProductD 'Unsafe] fareProds) expTime

baseKey :: Text
baseKey = "CachedQueries:FareProduct"

makeAllOrgIdKey :: Id Organization -> Text
makeAllOrgIdKey id = baseKey <> ":OrgId-" <> id.getId

makeAllOrgIdTypeKey :: Id Organization -> Maybe FareProductType -> Text
makeAllOrgIdTypeKey orgId mbFPType = baseKey <> ":OrgId-" <> orgId.getId <> ":Type-" <> fpTypeStr <> ":All"
  where
    fpTypeStr = maybe "None" show mbFPType

-- Call it after any update
clearCache :: HedisFlow m r => Id Organization -> FareProductType -> m ()
clearCache orgId fpType = do
  Hedis.del (makeAllOrgIdKey orgId)
  Hedis.del (makeAllOrgIdTypeKey orgId (Just fpType))
  Hedis.del (makeAllOrgIdTypeKey orgId Nothing)

insertIfNotExist ::
  Id Organization ->
  FareProductType ->
  Esq.SqlDB ()
insertIfNotExist = Queries.insertIfNotExist

delete ::
  Id Organization ->
  FareProductType ->
  Esq.SqlDB ()
delete = Queries.delete
