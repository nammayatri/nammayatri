{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RentalSlab where

import Domain.Types.RentalSlab
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.RentalSlab as BeamRS

createRentalSlab :: (L.MonadFlow m, Log m) => RentalSlab -> m ()
createRentalSlab = createWithKV

-- findById' :: (MonadThrow m, Log m, Transactionable m) => Id RentalSlab -> DTypeBuilder m (Maybe RentalSlabT)
-- findById' = Esq.findById'

findById :: (L.MonadFlow m, Log m) => Id RentalSlab -> m (Maybe RentalSlab)
findById rentalSlabId = findOneWithKV [Se.Is BeamRS.id $ Se.Eq (getId rentalSlabId)]

instance FromTType' BeamRS.RentalSlab RentalSlab where
  fromTType' BeamRS.RentalSlabT {..} = do
    pure $
      Just
        RentalSlab
          { id = Id id,
            baseDistance = baseDistance,
            baseDuration = baseDuration
          }

instance ToTType' BeamRS.RentalSlab RentalSlab where
  toTType' RentalSlab {..} = do
    BeamRS.RentalSlabT
      { BeamRS.id = getId id,
        BeamRS.baseDistance = baseDistance,
        BeamRS.baseDuration = baseDuration
      }
