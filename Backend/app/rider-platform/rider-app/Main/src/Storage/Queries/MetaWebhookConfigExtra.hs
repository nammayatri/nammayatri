{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | This file exists to satisfy EXTRA_QUERY_FILE, which routes
-- fromTType'/toTType' generation through the split OrphanInstances file
-- (needed for botConfig's JSONB decode to get correctly unwrapped with
-- fromMaybeM, mirroring RiderPreferencesExtra.hs).
--
-- findAll is hand-written here rather than via a YAML `where:` filter:
-- `enabled` is a boolean, and a secondary index on a low-cardinality column
-- doesn't narrow anything down (same reason this repo's constraint deny-list
-- blocks status-like fields from being secondary keys). Callers that only
-- want enabled rows filter in-code after fetching everything.
module Storage.Queries.MetaWebhookConfigExtra where

import Domain.Types.MetaWebhookConfig (MetaWebhookConfig)
import Kernel.Beam.Functions (findAllWithKV)
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
-- Instance-only import: pulls the FromTType'/ToTType' instances into the
-- build; nothing here is referenced by name, so plain `import X` would trip
-- -Wunused-imports under -Werror.

import qualified Storage.Beam.MetaWebhookConfig as Beam
import Storage.Queries.OrphanInstances.MetaWebhookConfig ()

-- | Covers both boolean values (so it's a no-op filter, matching "no filter"
-- semantics) rather than an empty condition list — GHC can't infer which
-- Beam table `findAllWithKV []` means without a Beam field reference
-- somewhere in the conditions to pin the type.
findAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m [MetaWebhookConfig]
findAll = findAllWithKV [Se.Or [Se.Is Beam.enabled $ Se.Eq True, Se.Is Beam.enabled $ Se.Eq False]]
