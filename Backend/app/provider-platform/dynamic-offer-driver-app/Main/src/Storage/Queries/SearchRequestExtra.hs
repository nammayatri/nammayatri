{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequestExtra where

import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RiderDetails as RD
import Domain.Types.SearchRequest as Domain
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.SearchRequest
import Tools.Error

createDSReq' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchRequest -> m ()
createDSReq' = createWithKV

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
  case searchRequest.toLocation of
    Just toLocation -> do
      toLocationMap <- SLM.buildDropLocationMapping toLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.providerId) (Just searchRequest.merchantOperatingCityId)
      QLM.create toLocationMap
    _ -> return ()
  create searchRequest

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
