module Storage.Queries.SearchRequestExtra where

import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.RiderDetails as RD
import Domain.Types.SearchRequest as Domain
import qualified EulerHS.Language as L
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
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

createDSReqFresh :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
createDSReqFresh searchRequest = do
  now <- getCurrentTime
  let entityId = searchRequest.id.getId
      stops = searchRequest.stops
      mkMapping locationId order = do
        id <- generateGUID
        pure
          DLM.LocationMapping
            { id,
              entityId,
              locationId,
              order,
              tag = DLM.SEARCH_REQUEST,
              version = QLM.latestTag,
              merchantId = Just searchRequest.providerId,
              merchantOperatingCityId = Just searchRequest.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
  fromMapping <- mkMapping searchRequest.fromLocation.id 0
  stopMappings <- zipWithM (\stop order -> mkMapping stop.id order) stops [1 ..]
  mbToMapping <- forM searchRequest.toLocation $ \toLocation -> mkMapping toLocation.id (length stops + 1)
  let locations = searchRequest.fromLocation : stops <> maybeToList searchRequest.toLocation
      mappings = fromMapping : stopMappings <> maybeToList mbToMapping
  runWritesConcurrently $
    map (\loc -> ("createDSReqFresh:location", QL.create loc)) locations
      <> map (\m -> ("createDSReqFresh:locationMapping", QLM.create m)) mappings
      <> [("createDSReqFresh:searchRequest", createDSReq' searchRequest)]

runWritesConcurrently :: (MonadFlow m) => [(Text, m ())] -> m ()
runWritesConcurrently writes = do
  awaitables <- forM writes $ \(tag, write) -> awaitableFork tag write
  forM_ (zip (map fst writes) awaitables) $ \(tag, awaitable) ->
    L.await Nothing awaitable >>= \case
      Right () -> pure ()
      Left err -> throwError $ InternalError $ tag <> " failed: " <> show err

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
            Se.Set BeamSR.preferSafetyPlus $ Just searchRequest.preferSafetyPlus,
            Se.Set BeamSR.isPetRide $ Just searchRequest.isPetRide
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

findRecentByRiderIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id RD.RiderDetails] -> Int -> m [SearchRequest]
findRecentByRiderIds riderIds limit =
  findAllFromKvRedis
    [Se.Is BeamSR.riderId $ Se.In (Just . getId <$> riderIds)]
    (Just $ Se.Desc BeamSR.createdAt)
    <&> take limit

-- findSearchRequestById ::
--   (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
--   [Id SearchRequest] ->
--   m [(Maybe DL.Location, Maybe DL.Location)]
-- findSearchRequestById srIds =
--   findAllWithKVSelect
--     [ Se.And [Se.Is BeamSR.id $ Se.In $ getId <$> srIds] ]
--     (\row -> (BeamSR.fromLocationId row, BeamSR.toLocationId row))
