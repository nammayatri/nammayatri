module Storage.Queries.Transformers.RouteDetails where

import Data.List (sortOn)
import qualified Domain.Types.RouteDetails as RD
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.RouteDetails as QRD

getJourneyRouteDetails :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> m [RD.RouteDetails]
getJourneyRouteDetails journeyLegId = do
  routeDetails <-
    case journeyLegId of
      Just jlId -> do
        QRD.findAllByJourneyLegId jlId
          >>= \case
            rd@(_ : _) -> return rd
            [] -> do
              logError $ "No route details found for journeyLegId: " <> jlId
              return []
      Nothing -> do
        logError "No route details found for null journeyLegId"
        return []
  return $ sortOn (.subLegOrder) routeDetails
