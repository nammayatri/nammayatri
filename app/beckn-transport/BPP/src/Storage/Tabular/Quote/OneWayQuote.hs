{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Centesimal
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import Storage.Tabular.Quote.Table

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayQuoteT sql=one_way_quote
      quoteId QuoteTId
      distance Centesimal
      distanceToNearestDriver Centesimal
      estimatedFinishTime UTCTime
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey OneWayQuoteT where
  type DomainKey OneWayQuoteT = Id Domain.Quote
  fromKey (OneWayQuoteTKey _id) = fromKey _id
  toKey id = OneWayQuoteTKey $ toKey id
