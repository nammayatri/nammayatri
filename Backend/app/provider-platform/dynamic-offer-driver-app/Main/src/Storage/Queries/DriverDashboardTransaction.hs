{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | KV-backed audit writes for this server's own action type.
--
-- Its own Beam type over the shared @transaction@ table, because @ToTType'@
-- carries a functional dependency @t -> a@: one Beam type determines exactly one
-- domain type. Same pattern as lib-dashboard's @Storage.Queries.Transaction@.
module Storage.Queries.DriverDashboardTransaction where

import qualified "this" Domain.Types.AccessMatrix as AccessMatrix
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import Kernel.Beam.Functions
import "lib-dashboard" Storage.Beam.BeamFlow (BeamFlow)
import qualified "this" Storage.Beam.DriverDashboardTransaction as BeamT
import qualified "lib-dashboard" Storage.Queries.Transaction as QT

instance ToTType' BeamT.DriverDashboardTransaction (DT.Transaction AccessMatrix.UserActionType) where
  toTType' txn = coerceRow (QT.toBeamTransaction txn)
    where
      -- structurally identical rows; only the Beam type tag differs
      coerceRow r =
        BeamT.DriverDashboardTransactionT
          { id = r.id,
            requestorId = r.requestorId,
            serverName = r.serverName,
            merchantId = r.merchantId,
            commonDriverId = r.commonDriverId,
            commonRideId = r.commonRideId,
            endpoint = r.endpoint,
            request = r.request,
            response = r.response,
            responseError = r.responseError,
            createdAt = r.createdAt
          }

create :: BeamFlow m r => DT.Transaction AccessMatrix.UserActionType -> m ()
create = createWithKV
