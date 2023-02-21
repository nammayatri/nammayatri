module Storage.Queries.Message.MediaFile where

import Domain.Types.Message.MediaFile
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Message.MediaFile

create :: MediaFile -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id MediaFile -> m (Maybe MediaFile)
findById = Esq.findById

findAllIn :: Transactionable m => [Id MediaFile] -> m [MediaFile]
findAllIn mfList =
  Esq.findAll $ do
    mediaFile <- from $ table @MediaFileT
    where_ $ mediaFile ^. MediaFileId `in_` valList (map getId mfList)
    return mediaFile
