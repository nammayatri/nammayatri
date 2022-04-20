{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FareProduct where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FareProduct as Domain
import qualified Storage.Tabular.Organization as TOrg

derivePersistField "Domain.FareProductType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareProductT sql=fare_product
      id Text
      organizationId TOrg.OrganizationTId
      productType Domain.FareProductType sql=type
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey FareProductT where
  type DomainKey FareProductT = Id Domain.FareProduct
  fromKey (FareProductTKey _id) = Id _id
  toKey (Id id) = FareProductTKey id

instance TEntity FareProductT Domain.FareProduct where
  fromTEntity entity = do
    let FareProductT {..} = entityVal entity
    return $
      Domain.FareProduct
        { id = Id id,
          organizationId = fromKey organizationId,
          _type = productType,
          ..
        }
  toTType Domain.FareProduct {..} =
    FareProductT
      { id = getId id,
        organizationId = toKey organizationId,
        productType = _type,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
