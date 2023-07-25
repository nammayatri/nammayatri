{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Feedback.Feedback where

import Domain.Types.Feedback.Feedback (Feedback)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Lib.Mesh as Mesh
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Feedback.Feedback as BeamF
import Storage.Tabular.Feedback.Feedback ()

create :: L.MonadFlow m Feedback -> m ()
create = createWithKV

createMany :: L.MonadFlow m => [Feedback] -> m ()
createMany = traverse_ create

instance FromTType' BeamF.Feedback Feedback where
  fromTType' BeamF.FeedbackT {..} = do
    pure $
      Just
        Feedback
          { id = Id id,
            rideId = Id rideId,
            driverId = Id driverId,
            badge = badge,
            createdAt = createdAt
          }

instance ToTType' BeamF.Feedback Feedback where
  toTType' Feedback {..} =
    BeamF.defaultFeedback
      { BeamF.id = getId id,
        BeamF.rideId = getId rideId,
        BeamF.driverId = getId driverId,
        BeamF.badge = badge,
        BeamF.createdAt = createdAt
      }
