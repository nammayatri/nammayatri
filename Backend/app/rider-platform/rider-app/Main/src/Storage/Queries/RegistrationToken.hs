{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RegistrationToken where

import Domain.Types.Person
import Domain.Types.RegistrationToken
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB m ()
create = Esq.create

findById :: forall m ma. Transactionable ma m => Id RegistrationToken -> Proxy ma -> m (Maybe RegistrationToken)
findById regTokenId _ = Esq.findById @m @ma regTokenId

findByToken :: forall m ma. Transactionable ma m => Text -> Proxy ma -> m (Maybe RegistrationToken)
findByToken token_ _ =
  findOne @m @ma $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenToken ==. val token_
    return registrationToken

setVerified :: Id RegistrationToken -> SqlDB m ()
setVerified rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenVerified =. val True
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

updateAttempts :: Int -> Id RegistrationToken -> SqlDB m ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenUpdatedAt =. val now,
        RegistrationTokenAttempts =. val attemps
      ]
    where_ $ tbl ^. RegistrationTokenId ==. val (getId rtId)

deleteByPersonId :: Id Person -> SqlDB m ()
deleteByPersonId (Id personId) = do
  delete $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ (registrationToken ^. RegistrationTokenEntityId ==. val personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB m ()
deleteByPersonIdExceptNew (Id personId) newRT = do
  delete $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $
      (registrationToken ^. RegistrationTokenEntityId ==. val personId)
        &&. not_ (registrationToken ^. RegistrationTokenId ==. val (getId newRT))

findAllByPersonId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m [RegistrationToken]
findAllByPersonId (Id personId) _ =
  findAll @m @ma $ do
    registrationToken <- from $ table @RegistrationTokenT
    where_ $ registrationToken ^. RegistrationTokenEntityId ==. val personId
    return registrationToken
