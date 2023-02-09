{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Webengage where

import Domain.Types.Webengage
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
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
