module Storage.Queries.Sos where

import Domain.Types.Person as Person ()
import Domain.Types.Sos as Sos
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Sos

create :: Sos.Sos -> SqlDB ()
create = Esq.create

updateStatus :: Id Sos.Sos -> Sos.SosStatus -> SqlDB ()
updateStatus sosId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SosUpdatedAt =. val now,
        SosStatus =. val status
      ]
    where_ $ tbl ^. SosId ==. val (getId sosId)

findById :: Transactionable m => Id Sos.Sos -> m (Maybe Sos)
findById = Esq.findById
