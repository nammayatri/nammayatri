{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.HelperFunction
  ( streamRideUpdates,
  )
where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)

data DriverRideUpdateStreamData = DriverRideUpdateStreamData
  { rId :: Maybe Text, -- rideId
    dId :: Text,
    cAt :: UTCTime, -- timestamp
    uAt :: UTCTime, -- systemtime when location update recieved
    rS :: DRide.RideStatus -- ride status
  }
  deriving (Generic, FromJSON, ToJSON)

streamRideUpdates ::
  ( MonadFlow m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["businessLogicUpdates" ::: Text]
  ) =>
  Maybe (Id DRide.Ride) ->
  Id Person.Person ->
  UTCTime ->
  UTCTime ->
  DRide.RideStatus ->
  m ()
streamRideUpdates mbRideId driverId createdAt updatedAt rideStatus = do
  let topicName = "businessLogicUpdates"
  produceMessage
    (topicName, Just (encodeUtf8 $ getId driverId))
    (DriverRideUpdateStreamData (getId <$> mbRideId) (getId driverId) createdAt updatedAt rideStatus)
