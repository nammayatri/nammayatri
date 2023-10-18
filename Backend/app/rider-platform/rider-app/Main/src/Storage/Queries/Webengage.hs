{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Webengage where

import Domain.Types.Webengage
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Webengage as BeamW

create :: MonadFlow m => Webengage -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Webengage -> m (Maybe Webengage)
findById webengageId = findOneWithKV [Se.Is BeamW.id $ Se.Eq (getId webengageId)]

findByInfoMsgId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Webengage)
findByInfoMsgId infoMessageId = findOneWithKV [Se.Is BeamW.infoMessageId $ Se.Eq infoMessageId]

instance FromTType' BeamW.Webengage Webengage where
  fromTType' BeamW.WebengageT {..} = do
    pure $
      Just
        Webengage
          { id = Id id,
            version = version,
            contentTemplateId = contentTemplateId,
            principalEntityId = principalEntityId,
            infoMessageId = infoMessageId,
            webMessageId = webMessageId,
            toNumber = toNumber,
            status = status
          }

instance ToTType' BeamW.Webengage Webengage where
  toTType' Webengage {..} = do
    BeamW.WebengageT
      { BeamW.id = getId id,
        BeamW.version = version,
        BeamW.contentTemplateId = contentTemplateId,
        BeamW.principalEntityId = principalEntityId,
        BeamW.infoMessageId = infoMessageId,
        BeamW.webMessageId = webMessageId,
        BeamW.toNumber = toNumber,
        BeamW.status = status
      }
