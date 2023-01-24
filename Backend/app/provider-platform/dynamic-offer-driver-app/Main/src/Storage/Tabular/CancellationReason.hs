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

module Storage.Tabular.CancellationReason where

import qualified Domain.Types.CancellationReason as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CancellationReasonT sql=cancellation_reason
      reasonCode Text
      description Text
      enabled Bool
      priority Int
      Primary reasonCode
      deriving Generic
    |]

instance TEntityKey CancellationReasonT where
  type DomainKey CancellationReasonT = Domain.CancellationReasonCode
  fromKey (CancellationReasonTKey _id) = Domain.CancellationReasonCode _id
  toKey (Domain.CancellationReasonCode id) = CancellationReasonTKey id

instance FromTType CancellationReasonT Domain.CancellationReason where
  fromTType CancellationReasonT {..} = do
    return $
      Domain.CancellationReason
        { reasonCode = Domain.CancellationReasonCode reasonCode,
          ..
        }

instance ToTType CancellationReasonT Domain.CancellationReason where
  toTType Domain.CancellationReason {..} =
    CancellationReasonT
      { reasonCode = let (Domain.CancellationReasonCode rc) = reasonCode in rc,
        ..
      }
