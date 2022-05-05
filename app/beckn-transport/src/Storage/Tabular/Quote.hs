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
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.Quote.OneWayQuote as QOneWayQuote
import Storage.Tabular.FareProduct ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Products (ProductsTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()
import Types.Error
import Utils.Common hiding (id)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      fareProductType Domain.FareProductType
      requestId SearchRequestTId
      productId ProductsTId
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      providerId OrganizationTId
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
    quoteDetails <- case fareProductType of
      Domain.RENTAL -> return Domain.RentalDetails
      Domain.ONE_WAY -> do
        oneWayQuoteEntity <- QOneWayQuote.findByQuoteId (Id id) >>= fromMaybeM QuoteDoesNotExist
        return . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distance = oneWayQuoteEntity.distance,
              distanceToNearestDriver = oneWayQuoteEntity.distance,
              ..
            }
    pure
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
        fareProductType = Domain.ONE_WAY,
        requestId = toKey requestId,
        productId = toKey productId,
        providerId = toKey providerId,
        ..
      }

  toTEntity quote =
    Entity (toKey quote.id) $ toTType quote
