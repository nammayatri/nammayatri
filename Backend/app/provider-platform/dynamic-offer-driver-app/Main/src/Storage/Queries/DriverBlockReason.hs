{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverBlockReason where

import Data.Function
import Domain.Types.DriverBlockReason
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.DriverBlockReason as BeamDBR

findAll :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [DriverBlockReason]
findAll = findAllWithKV [Se.Is BeamDBR.reasonCode $ Se.Not $ Se.Eq ""]

instance FromTType' BeamDBR.DriverBlockReason DriverBlockReason where
  fromTType' BeamDBR.DriverBlockReasonT {..} = do
    pure $
      Just
        DriverBlockReason
          { reasonCode = Id reasonCode,
            blockReason = blockReason,
            blockTimeInHours = blockTimeInHours
          }

instance ToTType' BeamDBR.DriverBlockReason DriverBlockReason where
  toTType' DriverBlockReason {..} = do
    BeamDBR.DriverBlockReasonT
      { BeamDBR.reasonCode = getId reasonCode,
        BeamDBR.blockReason = blockReason,
        BeamDBR.blockTimeInHours = blockTimeInHours
      }
