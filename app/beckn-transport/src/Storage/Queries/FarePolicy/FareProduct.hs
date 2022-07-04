{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareProduct where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.FareProduct
import Domain.Types.Organization (Organization)
import Storage.Tabular.FareProduct

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

upsertFareProduct ::
  Id Organization ->
  FareProductType ->
  SqlDB ()
upsertFareProduct orgId typ = do
  mbFp <- find predicate <$> findEnabledByOrgIdAndType (Just typ) orgId
  case mbFp of
    Nothing -> insertFareProduct
    Just _ -> pure ()
  where
    predicate :: FareProduct -> Bool
    predicate fp = fp._type == typ && fp.organizationId == orgId

    insertFareProduct :: SqlDB ()
    insertFareProduct = do
      now <- getCurrentTime
      guid <- Id <$> generateGUIDText
      Esq.create $
        FareProduct
          { id = guid,
            organizationId = orgId,
            _type = typ,
            createdAt = now
          }

deleteFareProduct ::
  Id Organization ->
  FareProductType ->
  SqlDB ()
deleteFareProduct orgId fpType = Esq.delete $ do
  fareProduct <- from $ table @FareProductT
  where_ $
    fareProduct ^. FareProductOrganizationId ==. val (toKey orgId)
      &&. fareProduct ^. FareProductProductType ==. val fpType
