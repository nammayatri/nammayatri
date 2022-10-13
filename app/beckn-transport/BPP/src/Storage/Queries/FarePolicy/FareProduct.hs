{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareProduct
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.FarePolicy.FareProduct
import Domain.Types.Organization (Organization)
import Storage.Tabular.FarePolicy.FareProduct

findEnabledByOrgId ::
  Transactionable m =>
  Id Organization ->
  m [FareProduct]
findEnabledByOrgId = findEnabledByOrgIdAndType Nothing

findEnabledByOrgIdAndType ::
  Transactionable m =>
  Maybe FareProductType ->
  Id Organization ->
  m [FareProduct]
findEnabledByOrgIdAndType mbType orgId =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductOrganizationId ==. val (toKey orgId)
        &&. whenJust_ mbType (\typ -> fareProduct ^. FareProductProductType ==. val typ)
    pure fareProduct

insertIfNotExist ::
  Id Organization ->
  FareProductType ->
  SqlDB ()
insertIfNotExist orgId typ = do
  mbFp <- listToMaybe <$> findEnabledByOrgIdAndType (Just typ) orgId
  case mbFp of
    Nothing -> insertFareProduct
    Just _ -> pure ()
  where
    insertFareProduct :: SqlDB ()
    insertFareProduct = do
      now <- getCurrentTime
      guid <- Id <$> generateGUIDText
      Esq.create @_ @FareProduct $
        FareProduct
          { id = guid,
            organizationId = orgId,
            _type = typ,
            createdAt = now
          }

delete ::
  Id Organization ->
  FareProductType ->
  SqlDB ()
delete orgId fpType = Esq.delete $ do
  fareProduct <- from $ table @FareProductT
  where_ $
    fareProduct ^. FareProductOrganizationId ==. val (toKey orgId)
      &&. fareProduct ^. FareProductProductType ==. val fpType
