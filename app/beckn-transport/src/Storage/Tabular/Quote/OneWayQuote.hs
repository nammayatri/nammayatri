{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Quote.OneWayQuote as Domain

-- FIXME quoteId should be QuoteTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayQuoteT sql=one_way_quote
      quoteId Text
      distance Double
      distanceToNearestDriver Double
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey OneWayQuoteT where
  type DomainKey OneWayQuoteT = Id Domain.Quote
  fromKey (OneWayQuoteTKey _id) = Id _id
  toKey (Id id) = OneWayQuoteTKey id

instance TEntity OneWayQuoteT Domain.OneWayQuote where
  fromTEntity entity = do
    let OneWayQuoteT {..} = entityVal entity
    return $
      Domain.OneWayQuote
        { quoteId = Id quoteId,
          distance = HighPrecMeters distance,
          distanceToNearestDriver = HighPrecMeters distanceToNearestDriver,
          ..
        }
  toTType Domain.OneWayQuote {..} =
    OneWayQuoteT
      { quoteId = getId quoteId,
        distance = getHighPrecMeters distance,
        distanceToNearestDriver = getHighPrecMeters distanceToNearestDriver,
        ..
      }
  toTEntity a =
    Entity (toKey a.quoteId) $ toTType a
