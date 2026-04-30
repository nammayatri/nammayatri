{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Toll.Storage.BeamFlow where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Utils.Common
import qualified Toll.Storage.Beam.Toll as BeamToll

-- | The constraint shared queries use. Each consuming app must provide an orphan
--   @HasSchemaName TollT@ instance pointing at its own schema (atlas_app for rider,
--   atlas_driver_offer_bpp for driver). Conventionally placed at
--   @Storage.Beam.Toll@ in the per-app source tree.
type BeamFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasSchemaName BeamToll.TollT
  )
