{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.CapabilityEndpoint where

import qualified Domain.Types.Capability as DC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.CapabilityEndpoint as BeamCE

-- | Server name under which the nammayatri dashboard registers its endpoints
-- in capability_endpoint. The control-center Express server uses its own.
dashboardServerName :: Text
dashboardServerName = "DASHBOARD"

-- | Every capability an endpoint is assigned to. An endpoint may be assigned to
-- several, so a narrow role can hold its own capability over an endpoint a
-- broader role also reaches. Callers must treat the result as ANY-of: holding
-- any one of the returned capabilities grants the call. An empty list means the
-- endpoint is unmapped, which denies.
findAllByEndpointId :: BeamFlow m r => Text -> m [DC.CapabilityEndpoint]
findAllByEndpointId endpointId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamCE.serverName $ Se.Eq dashboardServerName,
          Se.Is BeamCE.endpointId $ Se.Eq endpointId
        ]
    ]

findAllByCapabilityId :: BeamFlow m r => Id DC.Capability -> m [DC.CapabilityEndpoint]
findAllByCapabilityId capabilityId =
  findAllWithKV [Se.Is BeamCE.capabilityId $ Se.Eq $ getId capabilityId]

instance FromTType' BeamCE.CapabilityEndpoint DC.CapabilityEndpoint where
  fromTType' BeamCE.CapabilityEndpointT {..} = do
    return $
      Just
        DC.CapabilityEndpoint
          { capabilityId = Id capabilityId,
            ..
          }

instance ToTType' BeamCE.CapabilityEndpoint DC.CapabilityEndpoint where
  toTType' DC.CapabilityEndpoint {..} =
    BeamCE.CapabilityEndpointT
      { capabilityId = getId capabilityId,
        ..
      }
