module Storage.Queries.SavedReqLocation where

import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SavedReqLocation

create :: SavedReqLocation -> SqlDB ()
create = Esq.create

findAllByRiderId :: Transactionable m => Id Person -> m [SavedReqLocation]
findAllByRiderId perId =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId)
    orderBy [desc $ saveReqLocation ^. SavedReqLocationUpdatedAt]
    return saveReqLocation

deleteByRiderIdAndTag :: Id Person -> Text -> SqlDB ()
deleteByRiderIdAndTag perId addressTag = do
  Esq.delete $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
        &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)

findAllByRiderIdAndTag :: Transactionable m => Id Person -> Text -> m [SavedReqLocation]
findAllByRiderIdAndTag perId addressTag =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
        &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)
    return saveReqLocation

deleteAllByRiderId :: Id Person -> SqlDB ()
deleteAllByRiderId personId = do
  Esq.delete $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey personId))