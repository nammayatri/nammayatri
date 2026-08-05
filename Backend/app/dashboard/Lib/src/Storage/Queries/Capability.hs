{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Capability where

import qualified Domain.Types.Capability as DC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Capability as BeamC

findAll :: BeamFlow m r => m [DC.Capability]
findAll = findAllWithOptionsKV [Se.Is BeamC.id $ Se.Not $ Se.Eq ""] (Se.Asc BeamC.id) Nothing Nothing

findById :: BeamFlow m r => Id DC.Capability -> m (Maybe DC.Capability)
findById capabilityId = findOneWithKV [Se.Is BeamC.id $ Se.Eq $ getId capabilityId]

instance FromTType' BeamC.Capability DC.Capability where
  fromTType' BeamC.CapabilityT {..} = do
    return $
      Just
        DC.Capability
          { id = Id id,
            ..
          }

instance ToTType' BeamC.Capability DC.Capability where
  toTType' DC.Capability {..} =
    BeamC.CapabilityT
      { id = getId id,
        ..
      }
