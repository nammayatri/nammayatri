{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.RentalQuote where

import qualified Domain.Types.Quote as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.RentalFarePolicy (RentalFarePolicyTId)
import Storage.Tabular.Quote.Table

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalQuoteT sql=rental_quote
      quoteId QuoteTId
      rentalFarePolicyId RentalFarePolicyTId
      Primary quoteId
      deriving Generic
    |]

instance TEntityKey RentalQuoteT where
  type DomainKey RentalQuoteT = Id Domain.Quote
  fromKey (RentalQuoteTKey ((QuoteTKey _id))) = Id _id
  toKey (Id id) = RentalQuoteTKey $ QuoteTKey id
