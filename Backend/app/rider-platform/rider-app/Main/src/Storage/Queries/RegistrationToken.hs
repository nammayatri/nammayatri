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
