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
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.RegistrationToken

create :: RegistrationToken -> SqlDB m ()
create = Esq.create

findById :: forall m ma. Transactionable ma m => Id RegistrationToken -> Proxy ma -> m (Maybe RegistrationToken)
findById regId _ = Esq.findById @m @ma regId

setVerified :: Id RegistrationToken -> SqlDB m ()
setVerified rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenVerified =. val True,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

findByToken :: forall m ma. Transactionable ma m => RegToken -> Proxy ma -> m (Maybe RegistrationToken)
findByToken token _ =
  findOne @m @ma $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenToken ==. val token
    return regToken

updateAttempts :: Int -> Id RegistrationToken -> SqlDB m ()
updateAttempts attemps rtId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RegistrationTokenAttempts =. val attemps,
        RegistrationTokenUpdatedAt =. val now
      ]
    where_ $ tbl ^. RegistrationTokenTId ==. val (toKey rtId)

deleteByPersonId :: Id Person -> SqlDB m ()
deleteByPersonId personId =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)

deleteByPersonIdExceptNew :: Id Person -> Id RegistrationToken -> SqlDB m ()
deleteByPersonIdExceptNew personId newRT =
  Esq.delete $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $
      regToken ^. RegistrationTokenEntityId ==. val (getId personId)
        &&. not_ (regToken ^. RegistrationTokenTId ==. val (toKey newRT))

findAllByPersonId :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m [RegistrationToken]
findAllByPersonId personId _ =
  findAll @m @ma $ do
    regToken <- from $ table @RegistrationTokenT
    where_ $ regToken ^. RegistrationTokenEntityId ==. val (getId personId)
    return regToken
