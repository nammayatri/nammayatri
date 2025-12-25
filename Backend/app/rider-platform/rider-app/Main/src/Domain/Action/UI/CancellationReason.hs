{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the Licen            se, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CancellationReason
  ( list,
    CancellationReasonAPIEntity,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.CancellationReason as DCR
import Domain.Types.Extra.CancellationReason
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Utils.Common
import qualified Storage.Queries.CancellationReason as QCR

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeCancellationReasonAPIEntity :: DCR.CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity DCR.CancellationReason {..} =
  CancellationReasonAPIEntity {..}

list :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => DCR.CancellationStage -> m [CancellationReasonAPIEntity]
list cancStage = do
  map makeCancellationReasonAPIEntity <$> runInReplica (QCR.findAll cancStage)
