{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Products (ProductsTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      requestId SearchRequestTId
      productId ProductsTId
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      providerId OrganizationTId
      distance Double
      distanceToNearestDriver Double
      vehicleVariant Vehicle.Variant
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id

instance TEntity QuoteT Domain.Quote where
  fromTEntity entity = do
    let QuoteT {..} = entityVal entity
    return $
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          productId = fromKey productId,
          providerId = fromKey providerId,
          ..
        }
  toTType Domain.Quote {..} =
    QuoteT
      { id = getId id,
        requestId = toKey requestId,
        productId = toKey productId,
        providerId = toKey providerId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
