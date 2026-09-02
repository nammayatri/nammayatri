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
import qualified Domain.Types.ResourceScope as DRS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Capability as BeamC
import qualified Storage.Queries.CapabilityEndpoint as QCE

findAll :: BeamFlow m r => m [DC.Capability]
findAll = findAllWithOptionsKV [Se.Is BeamC.id $ Se.Not $ Se.Eq ""] (Se.Asc BeamC.id) Nothing Nothing

findById :: BeamFlow m r => Id DC.Capability -> m (Maybe DC.Capability)
findById capabilityId = findOneWithKV [Se.Is BeamC.id $ Se.Eq $ getId capabilityId]

-- | For an endpoint, its capabilities paired with each one's resourceType
-- (NULL = unscoped). Tools.Auth.Capability.enforceResourceScope uses this to
-- resolve scoping against the capabilities the caller actually holds: an
-- unscoped held capability lifts the restriction; otherwise the scoped type(s)
-- of the held capabilities are enforced.
endpointCapabilityTypes :: BeamFlow m r => Text -> m [(Text, Maybe DRS.ResourceType)]
endpointCapabilityTypes endpointId = do
  endpoints <- QCE.findAllByEndpointId endpointId
  caps <- catMaybes <$> mapM (findById . (.capabilityId)) endpoints
  pure $ map (\c -> (c.id.getId, c.resourceType)) caps

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
