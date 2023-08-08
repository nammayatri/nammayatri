{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person.DisabilityType
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Person ()
import Domain.Types.Person.DisabilityType
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.Person.DisabilityType as BeamDT

getAllDisabilities :: (L.MonadFlow m, Log m) => m [DisabilityType]
getAllDisabilities = findAllWithKV [Se.Is BeamDT.id $ Se.Not $ Se.Eq ""]

instance FromTType' BeamDT.DisabilityType DisabilityType where
  fromTType' BeamDT.DisabilityTypeT {..} = do
    pure $
      Just
        DisabilityType
          { id = Id id,
            tag = tag,
            subtag = subtag,
            description = description,
            onBookingMessage = onBookingMessage,
            onArrivalMessage = onArrivalMessage,
            onRideStartMessage = onRideStartMessage,
            createdAt = createdAt
          }

instance ToTType' BeamDT.DisabilityType DisabilityType where
  toTType' DisabilityType {..} = do
    BeamDT.DisabilityTypeT
      { BeamDT.id = getId id,
        BeamDT.tag = tag,
        BeamDT.subtag = subtag,
        BeamDT.description = description,
        BeamDT.onBookingMessage = onBookingMessage,
        BeamDT.onArrivalMessage = onArrivalMessage,
        BeamDT.onRideStartMessage = onRideStartMessage,
        BeamDT.createdAt = createdAt
      }
