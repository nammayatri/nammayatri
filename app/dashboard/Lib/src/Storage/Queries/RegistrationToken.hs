{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RegistrationToken where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person
import Domain.Types.RegistrationToken
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB ()
create = Esq.create

findByToken :: Transactionable m => RegToken -> m (Maybe RegistrationToken)
findByToken token =
  findOne $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenToken ==. val token
    return regToken

findAllByPersonId :: Transactionable m => Id Person -> m [RegistrationToken]
findAllByPersonId personId =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenPersonId ==. val (toKey personId)
    return regToken

findAllByPersonIdAndServerName :: Transactionable m => Id Person -> ServerName -> m [RegistrationToken]
findAllByPersonIdAndServerName personId serverName =
  findAll $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenPersonId ==. val (toKey personId)
        &&. regToken ^. RegistrationTokenServerName ==. val serverName
    return regToken

deleteAllByPersonId :: Id Person -> SqlDB ()
deleteAllByPersonId personId =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenPersonId ==. val (toKey personId)

deleteAllByPersonIdAndServerName :: Id Person -> ServerName -> SqlDB ()
deleteAllByPersonIdAndServerName personId serverName =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenPersonId ==. val (toKey personId)
        &&. regToken ^. RegistrationTokenServerName ==. val serverName

deleteById :: Id RegistrationToken -> SqlDB ()
deleteById = Esq.deleteByKey @RegistrationTokenT
