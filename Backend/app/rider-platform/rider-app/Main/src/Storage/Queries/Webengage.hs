{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Webengage where

import Domain.Types.Webengage
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.Webengage as BeamW
import Storage.Tabular.Webengage

create :: Webengage -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id Webengage -> m (Maybe Webengage)
findById = Esq.findById

findByInfoMsgId :: Transactionable m => Text -> m (Maybe Webengage)
findByInfoMsgId infoMessageId =
  Esq.findOne $ do
    webengage <- from $ table @WebengageT
    where_ $ webengage ^. WebengageInfoMessageId ==. val infoMessageId
    return webengage

transformBeamWebengageToDomain :: BeamW.Webengage -> Webengage
transformBeamWebengageToDomain BeamW.WebengageT {..} = do
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

transformDomainWebengageToBeam :: Webengage -> BeamW.Webengage
transformDomainWebengageToBeam Webengage {..} =
  BeamW.defaultWebengage
    { BeamW.id = getId id,
      BeamW.version = version,
      BeamW.contentTemplateId = contentTemplateId,
      BeamW.principalEntityId = principalEntityId,
      BeamW.infoMessageId = infoMessageId,
      BeamW.webMessageId = webMessageId,
      BeamW.toNumber = toNumber,
      BeamW.status = status
    }
