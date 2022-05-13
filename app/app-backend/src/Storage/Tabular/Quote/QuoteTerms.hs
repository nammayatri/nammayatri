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
import qualified Domain.Types.Quote.QuoteTerms as Domain

-- FIXME quoteId should be QuoteTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteTermsT sql=quote_terms
      id Text
      quoteId Text
      description Text
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteTermsT where
  type DomainKey QuoteTermsT = Id Domain.QuoteTermsEntity
  fromKey (QuoteTermsTKey _id) = Id _id
  toKey (Id id) = QuoteTermsTKey id

instance TEntity QuoteTermsT Domain.QuoteTermsEntity where
  fromTEntity entity = do
    let QuoteTermsT {..} = entityVal entity
    return $
      Domain.QuoteTermsEntity
        { id = Id id,
          quoteId = Id quoteId,
          ..
        }
  toTType Domain.QuoteTermsEntity {..} =
    QuoteTermsT
      { id = getId id,
        quoteId = getId quoteId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
