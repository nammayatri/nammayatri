module Storage.Queries.RegistrationToken where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.RegistrationToken
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = create'

findById :: EsqDBFlow m r => Id RegistrationToken -> m (Maybe RegistrationToken)
findById = Esq.findById

findByToken :: EsqDBFlow m r => Text -> m (Maybe RegistrationToken)
findByToken token_ =
  runTransaction . findOne' $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenToken ==. val token_
    return registrationToken

setVerified :: Id RegistrationToken -> SqlDB ()
setVerified rtId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenVerified =. val True
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

updateAttempts :: Int -> Id RegistrationToken -> SqlDB ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenAttempts =. val attemps
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId (Id personId) = do
  delete' $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ (registrationToken ^. RegistrationTokenEntityId ==. val personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB ()
deleteByPersonIdExceptNew (Id personId) newRT = do
  delete' $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $
      (registrationToken ^. RegistrationTokenEntityId ==. val personId)
        &&. not_ (registrationToken ^. RegistrationTokenId ==. val (getId newRT))

findAllByPersonId :: EsqDBFlow m r => Id Person -> m [RegistrationToken]
findAllByPersonId (Id personId) =
  runTransaction . findAll' $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenEntityId ==. val personId
    return registrationToken
