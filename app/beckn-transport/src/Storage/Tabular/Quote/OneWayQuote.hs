{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
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

instance TEntity OneWayQuoteT Domain.OneWayQuoteEntity where
  fromTEntity entity = do
    let OneWayQuoteT {..} = entityVal entity
    return $
      Domain.OneWayQuoteEntity
        { quoteId = Id quoteId,
          ..
        }
  toTType Domain.OneWayQuoteEntity {..} =
    OneWayQuoteT
      { quoteId = getId quoteId,
        ..
      }
  toTEntity a =
    Entity (toKey a.quoteId) $ toTType a
