module Storage.Queries.SearchRequestExtra where

import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.RiderDetails as RD
import Domain.Types.SearchRequest as Domain
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.SearchRequest ()

createDSReq' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
createDSReq' searchReq =
  if searchReq.isScheduled then createWithKVWithOptions Nothing True searchReq else createWithKV searchReq

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
create dsReq = do
  void $ whenNothingM_ (QL.findById dsReq.fromLocation.id) $ do QL.create dsReq.fromLocation
  case dsReq.toLocation of
    Just toLocation -> whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
    _ -> return ()
  createDSReq' dsReq

createDSReq :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
createDSReq searchRequest = do
  fromLocationMap <- SLM.buildPickUpLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.providerId) (Just searchRequest.merchantOperatingCityId)
  QLM.create fromLocationMap
  void $ createStopsLocation searchRequest.stops
  stopsLocMapping <- SLM.buildStopsLocationMapping searchRequest.stops searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.providerId) (Just searchRequest.merchantOperatingCityId)
  void $ QLM.createMany stopsLocMapping
  case searchRequest.toLocation of
    Just toLocation -> do
      toLocationMap <- SLM.buildDropLocationMapping toLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.providerId) (Just searchRequest.merchantOperatingCityId)
      QLM.create toLocationMap
    _ -> return ()
  create searchRequest

createStopsLocation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [DL.Location] -> m ()
createStopsLocation = QL.createMany

updateAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  Bool ->
  m ()
updateAutoAssign searchRequestId autoAssignedEnabled =
  updateOneWithKV
    [Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled]
    [Se.Is BeamSR.id (Se.Eq $ getId searchRequestId)]

updateRiderId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  Id RD.RiderDetails ->
  m ()
updateRiderId searchRequestId riderId =
  updateOneWithKV
    [Se.Set BeamSR.riderId $ Just $ getId riderId]
    [Se.Is BeamSR.id (Se.Eq $ getId searchRequestId)]

updateMultipleByRequestId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  SearchRequest ->
  Bool ->
  m ()
updateMultipleByRequestId searchRequest isScheduled =
  let updates =
        ( [ Se.Set BeamSR.riderId $ getId <$> searchRequest.riderId,
            Se.Set BeamSR.autoAssignEnabled searchRequest.autoAssignEnabled,
            Se.Set BeamSR.isAdvanceBookingEnabled $ Just searchRequest.isAdvanceBookingEnabled,
            Se.Set BeamSR.parcelType searchRequest.parcelType,
            Se.Set BeamSR.parcelQuantity searchRequest.parcelQuantity,
            Se.Set BeamSR.disabilityTag searchRequest.disabilityTag,
            Se.Set BeamSR.preferSafetyPlus $ Just searchRequest.preferSafetyPlus
          ]
        )
      condition = [Se.Is BeamSR.id (Se.Eq searchRequest.id.getId)]
   in if isScheduled
        then updateOneWithKVWithOptions Nothing True updates condition
        else updateOneWithKV updates condition

findSearchRequestById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id SearchRequest] -> m [SearchRequest]
findSearchRequestById srIds =
  findAllWithKV
    [ Se.And
        [Se.Is BeamSR.id $ Se.In $ getId <$> srIds]
    ]

-- findSearchRequestById ::
--   (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
--   [Id SearchRequest] ->
--   m [(Maybe DL.Location, Maybe DL.Location)]
-- findSearchRequestById srIds =
--   findAllWithKVSelect
--     [ Se.And [Se.Is BeamSR.id $ Se.In $ getId <$> srIds] ]
--     (\row -> (BeamSR.fromLocationId row, BeamSR.toLocationId row))
