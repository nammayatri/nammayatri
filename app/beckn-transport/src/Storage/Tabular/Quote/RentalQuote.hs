{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Quote.RentalQuote as Domain
import Storage.Tabular.RentalFarePolicy (RentalFarePolicyTId)

-- FIXME quoteId should be QuoteTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalQuoteT sql=rental_quote
      quoteId Text
      rentalFarePolicyId RentalFarePolicyTId
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
          rentalFarePolicyId = fromKey rentalFarePolicyId
        }
  toTType Domain.RentalQuote {..} =
    RentalQuoteT
      { quoteId = getId quoteId,
        rentalFarePolicyId = toKey rentalFarePolicyId
      }
  toTEntity a =
    Entity (toKey a.quoteId) $ toTType a
