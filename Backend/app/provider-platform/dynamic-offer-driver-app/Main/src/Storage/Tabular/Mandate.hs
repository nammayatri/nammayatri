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

module Storage.Tabular.Mandate where

import qualified Domain.Types.Mandate as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id

derivePersistField "Domain.MandateStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MandateT sql=mandate
      id Text
      status Domain.MandateStatus
      startDate UTCTime
      endDate UTCTime
      maxAmount HighPrecMoney
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey MandateT where
  type DomainKey MandateT = Id Domain.Mandate
  fromKey (MandateTKey _id) = Id _id
  toKey (Id id) = MandateTKey id

instance FromTType MandateT Domain.Mandate where
  fromTType MandateT {..} = do
    return $
      Domain.Mandate
        { id = Id id,
          ..
        }

instance ToTType MandateT Domain.Mandate where
  toTType Domain.Mandate {..} = do
    MandateT
      { id = getId id,
        ..
      }
