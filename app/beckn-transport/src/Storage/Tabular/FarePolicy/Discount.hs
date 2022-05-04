{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.Discount where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.Discount as Domain
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Vehicle as DVeh
import qualified Storage.Tabular.Organization as TOrg
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DiscountT sql=fare_policy_discount
      id Text
      vehicleVariant DVeh.Variant
      organizationId TOrg.OrganizationTId
      fareProductType DFareProduct.FareProductType
      fromDate UTCTime
      toDate UTCTime
      enabled Bool
      discount Double
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DiscountT where
  type DomainKey DiscountT = Id Domain.Discount
  fromKey (DiscountTKey _id) = Id _id
  toKey (Id id) = DiscountTKey id

instance TEntity DiscountT Domain.Discount where
  fromTEntity entity = do
    let DiscountT {..} = entityVal entity
    return $
      Domain.Discount
        { id = Id id,
          organizationId = fromKey organizationId,
          discount = toRational discount,
          ..
        }
  toTType Domain.Discount {..} =
    DiscountT
      { id = getId id,
        organizationId = toKey organizationId,
        discount = fromRational discount,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
