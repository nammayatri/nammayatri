{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Rating where

import Domain.Types.Ride as Ride
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.CallBAPInternal as CallBAPInternal
import qualified Storage.Queries.Ride as QRide
import Tools.Error

rating :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CoreMetrics m, HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal], CacheFlow m r) => CallBAPInternal.FeedbackReq -> m APISuccess
rating request = do
  appBackendBapInternal <- asks (.appBackendBapInternal)
  unless (request.ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == COMPLETED) $ throwError (RideInvalidStatus "Feedback available only for completed rides.")
  void $ CallBAPInternal.feedback appBackendBapInternal.apiKey appBackendBapInternal.url request
  pure Success
