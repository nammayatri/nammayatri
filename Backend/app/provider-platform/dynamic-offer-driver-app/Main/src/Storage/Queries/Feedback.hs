{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Feedback where

import Domain.Types.Feedback (Feedback)
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Feedback

create :: Feedback -> SqlDB ()
create = Esq.create

updateFeedback :: Id Feedback -> Id Person -> Text -> SqlDB ()
updateFeedback feedbackId driverId newBadge = do
  Esq.update $ \tbl -> do
    set
      tbl
      [FeedbackBadge =. val newBadge]
    where_ $
      tbl ^. FeedbackTId ==. val (toKey feedbackId)
        &&. tbl ^. FeedbackDriverId ==. val (toKey driverId)
