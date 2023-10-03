{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PublicTransportQuote where

import Domain.Types.PublicTransportQuote
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.PublicTransportQuote as BeamPTQ
import qualified Storage.Queries.PublicTransportQuote.RouteInfo as QueryPTQRI
import qualified Storage.Queries.PublicTransportQuote.Stop as QueryPTQS
import Tools.Error

createPublicTransportQuote :: MonadFlow m => PublicTransportQuote -> m ()
createPublicTransportQuote publicTransportQuote = do
  _ <- QueryPTQS.create publicTransportQuote.start
  _ <- QueryPTQS.create publicTransportQuote.end
  _ <- QueryPTQRI.create publicTransportQuote.routeInfo
  createWithKV publicTransportQuote

findById :: MonadFlow m => Id PublicTransportQuote -> m (Maybe PublicTransportQuote)
findById publicTransportQuoteId = findOneWithKV [Se.Is BeamPTQ.id $ Se.Eq (getId publicTransportQuoteId)]

instance FromTType' BeamPTQ.PublicTransportQuote PublicTransportQuote where
  fromTType' BeamPTQ.PublicTransportQuoteT {..} = do
    let stops = [] -- TODO : Fetch all stops using `stopIds`
    start <- QueryPTQS.findById (Id startStopId) >>= fromMaybeM (InternalError "No start-stop details")
    end <- QueryPTQS.findById (Id endStopId) >>= fromMaybeM (InternalError "No end-stop details")
    routeInfo <- QueryPTQRI.findById (Id routeInfoId) >>= fromMaybeM (InternalError "No route-info details")
    pure $
      Just
        PublicTransportQuote
          { id = Id id,
            quoteId = quoteId,
            ..
          }

instance ToTType' BeamPTQ.PublicTransportQuote PublicTransportQuote where
  toTType' PublicTransportQuote {..} = do
    let startId = start.id
        stopId = end.id
        routeInfoId = routeInfo.id
        stopIds = show $ map (.id) stops
    BeamPTQ.PublicTransportQuoteT
      { BeamPTQ.id = getId id,
        BeamPTQ.quoteId = quoteId,
        BeamPTQ.startStopId = getId startId,
        BeamPTQ.endStopId = getId stopId,
        BeamPTQ.routeInfoId = getId routeInfoId,
        BeamPTQ.totalEstimatedDistance = totalEstimatedDistance,
        BeamPTQ.totalDuration = totalDuration,
        BeamPTQ.vehicleServiceType = vehicleServiceType,
        BeamPTQ.stopIds = stopIds,
        BeamPTQ.createdAt = createdAt
      }
