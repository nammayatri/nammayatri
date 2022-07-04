{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.QuoteTerms where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Storage.Tabular.Quote.Table (QuoteTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteTermsT sql=quote_terms
      id Text
      quoteId QuoteTId
      description Text
      Primary id
      deriving Generic
    |]

data QuoteTerms

instance TEntityKey QuoteTermsT where
  type DomainKey QuoteTermsT = Id QuoteTerms
  fromKey (QuoteTermsTKey _id) = Id _id
  toKey (Id id) = QuoteTermsTKey id
