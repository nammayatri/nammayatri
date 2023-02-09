module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id RegistrationToken -> m (Maybe RegistrationToken)
findById = Esq.findById

findByToken :: Transactionable m => Text -> m (Maybe RegistrationToken)
findByToken token_ =
  findOne $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenToken ==. val token_
    return registrationToken

setVerified :: Id RegistrationToken -> SqlDB ()
setVerified rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenVerified =. val True
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenAttempts =. val attemps
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId (Id personId) = do
  delete $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ (registrationToken ^. RegistrationTokenEntityId ==. val personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
deleteByPersonIdExceptNew (Id personId) newRT = do
  delete $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $
      (registrationToken ^. RegistrationTokenEntityId ==. val personId)
        &&. not_ (registrationToken ^. RegistrationTokenId ==. val (getId newRT))

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId (Id personId) =
  findAll $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenEntityId ==. val personId
    return registrationToken
