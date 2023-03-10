{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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

instance FromTType IssueT Domain.Issue where
  fromTType IssueT {..} = do
    return $
      Domain.Issue
        { id = Id id,
          customerId = fromKey customerId,
          bookingId = fromKey <$> bookingId,
          ..
        }

instance ToTType IssueT Domain.Issue where
  toTType Domain.Issue {..} =
    IssueT
      { id = getId id,
        customerId = toKey customerId,
        bookingId = toKey <$> bookingId,
        ..
      }
