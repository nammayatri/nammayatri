{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Sos
  ( SosReq (..),
    SosRes (..),
    SosFeedbackReq (..),
    createSosDetails,
    updateSosDetails,
  )
where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Sos as DSos
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Sos as QSos
import Tools.Error

data SosReq = SosReq
  { flow :: DSos.SosType,
    rideId :: Id DRide.Ride
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype SosFeedbackReq = SosFeedbackReq
  { status :: DSos.SosStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype SosRes = SosRes
  { sosId :: Id DSos.Sos
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createSosDetails :: (EsqDBFlow m r, EncFlow m r) => Id Person.Person -> SosReq -> m SosRes
createSosDetails personId req = do
  sosDetails <- buildSosDetails personId req
  _ <- QSos.create sosDetails
  return $
    SosRes
      { sosId = sosDetails.id
      }

updateSosDetails :: (CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r) => Id DSos.Sos -> Id Person.Person -> SosFeedbackReq -> m APISuccess.APISuccess
updateSosDetails sosId personId req = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)

  unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"

  void $ QSos.updateStatus sosId (req.status)
  pure APISuccess.Success

buildSosDetails :: (EncFlow m r) => Id Person.Person -> SosReq -> m DSos.Sos
buildSosDetails personId req = do
  pid <- generateGUID
  now <- getCurrentTime
  return
    DSos.Sos
      { id = pid,
        personId = personId,
        createdAt = now,
        updatedAt = now,
        status = DSos.Pending,
        flow = req.flow,
        rideId = req.rideId
      }
