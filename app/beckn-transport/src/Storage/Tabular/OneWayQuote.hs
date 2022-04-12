{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.OneWayQuote as Domain
import Storage.Tabular.Quote (QuoteTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayQuoteT sql=one_way_quote
      id Text
      quoteId QuoteTId
      distance Double
      distanceToNearestDriver Double
      Primary id
      deriving Generic
    |]

instance TEntityKey OneWayQuoteT where
  type DomainKey OneWayQuoteT = Id Domain.OneWayQuote
  fromKey (OneWayQuoteTKey _id) = Id _id
  toKey (Id id) = OneWayQuoteTKey id

instance TEntity OneWayQuoteT Domain.OneWayQuote where
  fromTEntity entity = do
    let OneWayQuoteT {..} = entityVal entity
    return $
      Domain.OneWayQuote
        { id = Id id,
          quoteId = fromKey quoteId,
          ..
        }
  toTType Domain.OneWayQuote {..} =
    OneWayQuoteT
      { id = getId id,
        quoteId = toKey quoteId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
