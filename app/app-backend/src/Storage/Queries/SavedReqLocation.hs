module Storage.Queries.SavedReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import Storage.Tabular.SavedReqLocation

create :: SavedReqLocation -> SqlDB ()
create = create'

findAllByRiderId :: Transactionable m => Id Person -> m [SavedReqLocation]
findAllByRiderId perId =
  Esq.findAll $ do
    saveReqLocation <- from $ table @SavedReqLocationT
    where_ $
      saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId)
    return saveReqLocation

deleteByRiderIdAndTag :: Id Person -> Text -> SqlDB ()
deleteByRiderIdAndTag perId addressTag = do
  delete' $ do
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
