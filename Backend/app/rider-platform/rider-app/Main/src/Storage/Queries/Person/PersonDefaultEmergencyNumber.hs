{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person.PersonDefaultEmergencyNumber where

import Domain.Types.Person
import Domain.Types.Person.PersonDefaultEmergencyNumber
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Person.PersonDefaultEmergencyNumber

replaceAll :: Id Person -> [PersonDefaultEmergencyNumber] -> SqlDB ()
replaceAll personId pdenList = do
  Esq.delete $ do
    personENT <- from $ table @PersonDefaultEmergencyNumberT
    where_ $ personENT ^. PersonDefaultEmergencyNumberTId ==. val (toKey personId)
  Esq.createMany pdenList

findAllByPersonId ::
  Transactionable m =>
  Id Person ->
  m [PersonDefaultEmergencyNumber]
findAllByPersonId personId =
  Esq.findAll $ do
    personENT <- from $ table @PersonDefaultEmergencyNumberT
    where_ $
      personENT ^. PersonDefaultEmergencyNumberTId ==. val (toKey personId)
    return personENT
