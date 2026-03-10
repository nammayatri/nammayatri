{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
  You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.StateEntryPermitChargesExtra where

import Domain.Types.StateEntryPermitCharges
import Kernel.Beam.Functions (findAllWithKV)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.StateEntryPermitCharges as Beam
import Storage.Queries.OrphanInstances.StateEntryPermitCharges ()

-- Same pattern as FeedbackFormExtra.findAllFeedback, DriverBlockReasonExtra.findAll, MerchantExtra.findAll:
-- "find all" cannot be expressed in YAML (generator requires a where clause), so it lives in Extra with a dummy always-true condition.
-- Uses runInReplica + findAllWithKV so the read goes to replica and can use KV/caching as intended.
findAllStateEntryPermitCharges ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  m [StateEntryPermitCharges]
findAllStateEntryPermitCharges = B.runInReplica $ findAllWithKV [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
