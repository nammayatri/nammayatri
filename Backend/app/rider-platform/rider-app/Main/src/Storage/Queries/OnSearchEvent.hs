{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.OnSearchEvent where

import Domain.Types.OnSearchEvent
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Beam.OnSearchEvent as BeamOSE

create :: MonadFlow m => OnSearchEvent -> m ()
create = createWithKV

instance FromTType' BeamOSE.OnSearchEvent OnSearchEvent where
  fromTType' BeamOSE.OnSearchEventT {..} = do
    pure $
      Just
        OnSearchEvent
          { id = Id id,
            bppId = bppId,
            messageId = messageId,
            errorCode = errorCode,
            errorType = errorType,
            errorMessage = errorMessage,
            createdAt = createdAt
          }

instance ToTType' BeamOSE.OnSearchEvent OnSearchEvent where
  toTType' OnSearchEvent {..} = do
    BeamOSE.OnSearchEventT
      { BeamOSE.id = getId id,
        BeamOSE.bppId = bppId,
        BeamOSE.messageId = messageId,
        BeamOSE.errorCode = errorCode,
        BeamOSE.errorType = errorType,
        BeamOSE.errorMessage = errorMessage,
        BeamOSE.createdAt = createdAt
      }
