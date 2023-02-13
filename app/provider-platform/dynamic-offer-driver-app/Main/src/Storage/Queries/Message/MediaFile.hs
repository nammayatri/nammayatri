module Storage.Queries.Message.MediaFile where

import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Domain.Types.Message.MediaFile
import Storage.Tabular.Message.MediaFile ()
import Kernel.Types.Id
import Kernel.Prelude

create :: MediaFile -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id MediaFile -> m (Maybe MediaFile)
findById = Esq.findById
