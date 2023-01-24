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

module Storage.Tabular.RentalSlab where

import qualified Domain.Types.RentalSlab as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalSlabT sql=rental_slab
      id Text
      baseDistance Kilometers
      baseDuration Hours
      Primary id
      deriving Generic
    |]

instance TEntityKey RentalSlabT where
  type DomainKey RentalSlabT = Id Domain.RentalSlab
  fromKey (RentalSlabTKey _id) = Id _id
  toKey (Id id) = RentalSlabTKey id

instance FromTType RentalSlabT Domain.RentalSlab where
  fromTType RentalSlabT {..} =
    return $
      Domain.RentalSlab
        { id = Id id,
          ..
        }

instance ToTType RentalSlabT Domain.RentalSlab where
  toTType Domain.RentalSlab {..} =
    RentalSlabT
      { id = getId id,
        ..
      }
