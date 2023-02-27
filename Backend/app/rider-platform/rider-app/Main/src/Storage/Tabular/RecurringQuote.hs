{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RecurringQuote where

import qualified Domain.Types.RecurringQuote as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMeters, HighPrecMoney)
import Kernel.Types.Id
import qualified Storage.Tabular.SearchRequest as SSearchRequest

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RecurringQuoteT sql=recurring_quote
      id Text
      requestId SSearchRequest.SearchRequestTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      providerId Text
      providerUrl Text
      distanceToNearestDriver HighPrecMeters Maybe
      vehicleVariant VehVar.VehicleVariant
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RecurringQuoteT where
  type DomainKey RecurringQuoteT = Id Domain.RecurringQuote
  fromKey (RecurringQuoteTKey _id) = Id _id
  toKey (Id id) = RecurringQuoteTKey id
