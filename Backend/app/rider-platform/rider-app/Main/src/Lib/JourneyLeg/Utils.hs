{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.JourneyLeg.Utils where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Types as JL
import qualified Lib.JourneyModule.Types as JMTypes
import qualified Storage.Queries.JourneyRouteDetails as QJourneyRouteDetails
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.WalkLegMultimodal as QWalkLeg

markLegStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe JL.JourneyLegStatus -> JMTypes.LegInfo -> m ()
markLegStatus mbStatus journeyLegInfo = do
  let subLegOrders = getSubLegOrders journeyLegInfo.legExtraInfo
  when (null subLegOrders) $ logError $ "I'm empty, Search ID: " <> journeyLegInfo.searchId
  mapM_
    ( \subLegOrder ->
        case journeyLegInfo.legExtraInfo of
          JMTypes.Metro _ -> QJourneyRouteDetails.updateJourneyStatus mbStatus (Id journeyLegInfo.searchId) (Just subLegOrder)
          JMTypes.Subway _ -> QJourneyRouteDetails.updateJourneyStatus mbStatus (Id journeyLegInfo.searchId) (Just subLegOrder)
          JMTypes.Bus _ -> QJourneyRouteDetails.updateJourneyStatus mbStatus (Id journeyLegInfo.searchId) (Just subLegOrder)
          JMTypes.Walk legExtraInfo ->
            case mbStatus of
              Just status -> QWalkLeg.updateStatus (JMTypes.castWalkLegStatusFromLegStatus status) legExtraInfo.id
              Nothing -> return ()
          JMTypes.Taxi _ -> QSearchRequest.updateJourneyStatus mbStatus (Id journeyLegInfo.searchId)
    )
    subLegOrders

getSubLegOrders :: JMTypes.LegExtraInfo -> [Int]
getSubLegOrders legExtraInfo =
  case legExtraInfo of
    JMTypes.Metro metroInfo -> mapMaybe (.subOrder) metroInfo.routeInfo
    JMTypes.Subway subwayInfo -> mapMaybe (.subOrder) subwayInfo.routeInfo
    JMTypes.Bus busInfo -> mapMaybe (.subOrder) busInfo.routeInfo
    _ -> []
