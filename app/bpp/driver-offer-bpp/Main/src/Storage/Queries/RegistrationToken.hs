module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById = Esq.findById

setVerified :: Id RegistrationToken -> SqlDB ()
setVerified rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenVerified =. val True,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

findByToken :: Transactionable m => RegToken -> m (Maybe RegistrationToken)
findByToken token =
  findOne $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenToken ==. val token
    return regToken

updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenAttempts =. val attemps,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
deleteByPersonIdExceptNew personId newRT =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenEntityId ==. val (getId personId)
        &&. not_ (regToken ^. RegistrationTokenTId ==. val (toKey newRT))

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId personId =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)
    return regToken
