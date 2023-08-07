{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Mandate where

import Domain.Types.Mandate
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Mandate

findById :: Transactionable m => Id Mandate -> m (Maybe Mandate)
findById = Esq.findById

findByStatus :: Transactionable m => Id Mandate -> [MandateStatus] -> m (Maybe Mandate)
findByStatus mandateId status =
  Esq.findOne $ do
    mandate <- from $ table @MandateT
    where_ $
      mandate ^. MandateTId ==. val (toKey mandateId)
        &&. mandate ^. MandateStatus `in_` (valList status)
    return mandate

updateStatus :: Id Mandate -> MandateStatus -> SqlDB ()
updateStatus mandateId status = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ MandateStatus =. val status
      ]
    where_ $ tbl ^. MandateTId ==. val (toKey mandateId)
