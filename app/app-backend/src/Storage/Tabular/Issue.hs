{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Issue as Domain
import qualified Storage.Tabular.Person as SPerson
import qualified Storage.Tabular.Quote as SQuote

--FIXME: rideBookingId SQuote.QuoteTId Maybe
mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueT sql=issue
      id Text
      customerId SPerson.PersonTId
      rideBookingId SQuote.QuoteTId Maybe
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

instance TEntity IssueT Domain.Issue where
  fromTEntity entity = do
    let IssueT {..} = entityVal entity
    return $
      Domain.Issue
        { id = Id id,
          customerId = fromKey customerId,
          rideBookingId = fromKey <$> rideBookingId,
          ..
        }
  toTType Domain.Issue {..} =
    IssueT
      { id = getId id,
        customerId = toKey customerId,
        rideBookingId = toKey <$> rideBookingId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
