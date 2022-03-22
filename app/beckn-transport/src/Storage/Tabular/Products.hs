{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Products where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Products as Domain

derivePersistField "Domain.ProductsType"
derivePersistField "Domain.ProductsStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ProductsT sql=product
      id Text
      name Text
      description Text Maybe
      productsType Domain.ProductsType sql=type
      shortId Text
      status Domain.ProductsStatus
      price Amount
      rating Text Maybe
      review Text Maybe
      info Text Maybe
      udf1 Text Maybe
      udf2 Text Maybe
      udf3 Text Maybe
      udf4 Text Maybe
      udf5 Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey ProductsT where
  type DomainKey ProductsT = Id Domain.Products
  fromKey (ProductsTKey _id) = Id _id
  toKey (Id id) = ProductsTKey id

instance TEntity ProductsT Domain.Products where
  fromTEntity entity = do
    let ProductsT {..} = entityVal entity
    return $
      Domain.Products
        { id = Id id,
          shortId = ShortId shortId,
          _type = productsType,
          ..
        }
  toTType Domain.Products {..} =
    ProductsT
      { id = getId id,
        shortId = getShortId shortId,
        productsType = _type,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
