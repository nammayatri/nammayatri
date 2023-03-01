{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Message.MediaFile where

import Domain.Types.Message.MediaFile
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Message.MediaFile

create :: MediaFile -> SqlDB m ()
create = Esq.create

findById :: forall m ma. Transactionable ma m => Proxy ma -> Id MediaFile -> m (Maybe MediaFile)
findById _ = Esq.findById @m @ma

findAllIn :: forall m ma. Transactionable ma m => [Id MediaFile] -> Proxy ma -> m [MediaFile]
findAllIn mfList _ =
  Esq.findAll @m @ma $ do
    mediaFile <- from $ table @MediaFileT
    where_ $ mediaFile ^. MediaFileId `in_` valList (map getId mfList)
    return mediaFile
