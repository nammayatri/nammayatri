module Storage.Queries.FRFSSearchExtra where

import Control.Applicative ((<|>))
import Domain.Types.FRFSRouteDetails
import Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as FS
import Domain.Types.Journey
import qualified Domain.Types.JourneyRouteDetails as JourneyRouteDetails
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Lib.JourneyLeg.Types as JLS
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam
import qualified Storage.Queries.JourneyRouteDetails as JRD
import Storage.Queries.OrphanInstances.FRFSSearch ()
import qualified Storage.Queries.RouteDetails as QRD

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create frfsSearchReq = do
  forM_ (FS.journeyRouteDetails frfsSearchReq) $ \journeyRouteDetail -> do
    QRD.create journeyRouteDetail
    -- TODO :: This Table Creation can be removed as replaced with `RouteDetails`, is only for Backward Compatibility
    _now <- getCurrentTime
    newId <- Common.generateGUID
    let journeyRouteDetails =
          JourneyRouteDetails.JourneyRouteDetails
            { id = newId,
              lineColor = journeyRouteDetail.routeColorName,
              lineColorCode = journeyRouteDetail.routeColorCode,
              journeyStatus = Just JLS.InPlan,
              frequency = journeyRouteDetail.frequency,
              alternateShortNames = Just journeyRouteDetail.alternateShortNames,
              routeLongName = journeyRouteDetail.routeLongName,
              searchId = frfsSearchReq.id,
              platformNumber = journeyRouteDetail.fromStopPlatformCode,
              subLegOrder = journeyRouteDetail.subLegOrder,
              fromStationCode = journeyRouteDetail.fromStopCode <|> (journeyRouteDetail.fromStopGtfsId <&> gtfsIdtoDomainCode),
              toStationCode = journeyRouteDetail.toStopCode <|> (journeyRouteDetail.toStopGtfsId <&> gtfsIdtoDomainCode),
              routeCode = journeyRouteDetail.routeCode,
              merchantId = Just frfsSearchReq.merchantId,
              merchantOperatingCityId = Just frfsSearchReq.merchantOperatingCityId,
              createdAt = _now,
              updatedAt = _now
            }
    JRD.create journeyRouteDetails

  create' frfsSearchReq

findAllByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m [Domain.Types.FRFSSearch.FRFSSearch]
findAllByJourneyId journeyId =
  findAllWithKVAndConditionalDB
    [Se.Is Beam.journeyId $ Se.Eq (Just journeyId.getId)]
    Nothing

updatePricingId :: (MonadFlow m, EsqDBFlow m r) => Id FRFSSearch -> Maybe Text -> m ()
updatePricingId (Id reqId) pricingId = do
  updateOneWithKV
    [Se.Set Beam.pricingId pricingId]
    [Se.Is Beam.id (Se.Eq reqId)]

updateSkipBooking :: (MonadFlow m, EsqDBFlow m r) => Id FRFSSearch -> Maybe Bool -> m ()
updateSkipBooking (Id reqId) skipBooking = do
  updateOneWithKV
    [Se.Set Beam.skipBooking skipBooking]
    [Se.Is Beam.id (Se.Eq reqId)]

updateIsCancelled :: (MonadFlow m, EsqDBFlow m r) => Id FRFSSearch -> Maybe Bool -> m ()
updateIsCancelled (Id reqId) isDeleted = do
  updateOneWithKV
    [Se.Set Beam.isDeleted isDeleted]
    [Se.Is Beam.id (Se.Eq reqId)]

updateOnSearchFailed :: (MonadFlow m, EsqDBFlow m r) => Id FRFSSearch -> Maybe Bool -> m ()
updateOnSearchFailed (Id reqId) onSearchFailed = do
  updateOneWithKV
    [Se.Set Beam.onSearchFailed onSearchFailed]
    [Se.Is Beam.id (Se.Eq reqId)]
