{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue where

import qualified Domain.Types.Issue as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote

--FIXME: bookingId SQuote.QuoteTId Maybe
mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueT sql=issue
      id Text
      customerId SPerson.PersonTId
      bookingId SQuote.QuoteTId Maybe
      contactEmail Text Maybe
      reason Text
      description Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey IssueT where
  type DomainKey IssueT = Id Domain.Issue
  fromKey (IssueTKey _id) = Id _id
  toKey (Id id) = IssueTKey id

instance TType IssueT Domain.Issue where
  fromTType IssueT {..} = do
    return $
      Domain.Issue
        { id = Id id,
          customerId = fromKey customerId,
          bookingId = fromKey <$> bookingId,
          ..
        }
  toTType Domain.Issue {..} =
    IssueT
      { id = getId id,
        customerId = toKey customerId,
        bookingId = toKey <$> bookingId,
        ..
      }
