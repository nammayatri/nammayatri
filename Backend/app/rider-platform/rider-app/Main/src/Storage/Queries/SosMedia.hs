{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SosMedia where

import qualified Data.Time as T
import Domain.Types.Sos as DTS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.SosMedia as BeamSM

create :: MonadFlow m => DTS.SosMedia -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SosMedia -> m (Maybe SosMedia)
findById (Id sosMediaId) = findOneWithKV [Se.Is BeamSM.id $ Se.Eq sosMediaId]

findAllIn :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Id SosMedia] -> m [SosMedia]
findAllIn mfList = findAllWithKV [Se.Is BeamSM.id $ Se.In $ getId <$> mfList]

deleteById :: (MonadFlow m, CacheFlow m r) => Id SosMedia -> m ()
deleteById (Id sosMediaId) = deleteWithKV [Se.Is BeamSM.id (Se.Eq sosMediaId)]

instance FromTType' BeamSM.SosMedia SosMedia where
  fromTType' BeamSM.SosMediaT {..} = do
    pure $
      Just
        SosMedia
          { id = Id id,
            _type = fileType,
            url = url,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamSM.SosMedia SosMedia where
  toTType' SosMedia {..} = do
    BeamSM.SosMediaT
      { BeamSM.id = getId id,
        BeamSM.fileType = _type,
        BeamSM.url = url,
        BeamSM.createdAt = T.utcToLocalTime T.utc createdAt
      }
