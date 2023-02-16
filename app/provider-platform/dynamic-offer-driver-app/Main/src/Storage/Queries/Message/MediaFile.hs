module Storage.Queries.Message.MediaFile where

import Domain.Types.Message.MediaFile
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Message.MediaFile ()

create :: MediaFile -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id MediaFile -> m (Maybe MediaFile)
findById = Esq.findById
