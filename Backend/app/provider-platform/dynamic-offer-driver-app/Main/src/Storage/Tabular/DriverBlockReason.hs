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

module Storage.Tabular.DriverBlockReason where

import qualified Domain.Types.DriverBlockReason as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverBlockReasonT sql=driver_block_reason
      reasonCode Text
      blockReason Text Maybe
      blockTimeInHours Int Maybe
      Primary reasonCode
      deriving Generic
    |]

instance TEntityKey DriverBlockReasonT where
  type DomainKey DriverBlockReasonT = Domain.BlockReasonCode
  fromKey (DriverBlockReasonTKey _id) = Domain.BlockReasonCode _id
  toKey (Domain.BlockReasonCode id) = DriverBlockReasonTKey id

instance FromTType DriverBlockReasonT Domain.DriverBlockReason where
  fromTType DriverBlockReasonT {..} = do
    return $
      Domain.DriverBlockReason
        { reasonCode = Domain.BlockReasonCode reasonCode,
          ..
        }

instance ToTType DriverBlockReasonT Domain.DriverBlockReason where
  toTType Domain.DriverBlockReason {..} =
    DriverBlockReasonT
      { reasonCode = let (Domain.BlockReasonCode rc) = reasonCode in rc,
        ..
      }
