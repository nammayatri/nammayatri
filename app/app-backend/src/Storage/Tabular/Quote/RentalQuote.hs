{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Quote.RentalQuote as Domain

-- FIXME quoteId should be QuoteTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalQuoteT sql=rental_quote
      quoteId Text
      baseDistance Int
      baseDuration Int
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey RentalQuoteT where
  type DomainKey RentalQuoteT = Id Domain.Quote
  fromKey (RentalQuoteTKey _id) = Id _id
  toKey (Id id) = RentalQuoteTKey id

instance TEntity RentalQuoteT Domain.RentalQuote where
  fromTEntity entity = do
    let RentalQuoteT {..} = entityVal entity
    return $
      Domain.RentalQuote
        { quoteId = Id quoteId,
          baseDistance = Kilometers baseDistance,
          baseDuration = Hours baseDuration,
          ..
        }
  toTType Domain.RentalQuote {..} =
    RentalQuoteT
      { quoteId = getId quoteId,
        baseDistance = getKilometers baseDistance,
        baseDuration = getHours baseDuration,
        ..
      }
  toTEntity a =
    Entity (toKey a.quoteId) $ toTType a
