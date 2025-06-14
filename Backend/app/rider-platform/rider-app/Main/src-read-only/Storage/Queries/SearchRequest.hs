{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequest (module Storage.Queries.SearchRequest, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.Trip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as Beam
import Storage.Queries.SearchRequestExtra as ReExport

updateAdvancedBookingEnabled :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateAdvancedBookingEnabled isAdvanceBookingEnabled id = do updateOneWithKV [Se.Set Beam.isAdvanceBookingEnabled isAdvanceBookingEnabled] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAllJourneysLoaded :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateAllJourneysLoaded allJourneysLoaded id = do updateOneWithKV [Se.Set Beam.allJourneysLoaded allJourneysLoaded] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateHasMultimodalSearch :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateHasMultimodalSearch hasMultimodalSearch id = do updateOneWithKV [Se.Set Beam.hasMultimodalSearch hasMultimodalSearch] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateInitiatedBy :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Domain.Types.Trip.TripParty -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateInitiatedBy initiatedBy id = do updateOneWithKV [Se.Set Beam.initiatedBy initiatedBy] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePetRide :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updatePetRide isPetRide id = do updateOneWithKV [Se.Set Beam.isPetRide isPetRide] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRiderPreferredOption :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRequest.RiderPreferredOption -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
updateRiderPreferredOption riderPreferredOption id = do updateOneWithKV [Se.Set Beam.riderPreferredOption (Just riderPreferredOption)] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTotalRidesCount ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateTotalRidesCount id totalRidesCount riderId = do
  updateOneWithKV
    [ Se.Set Beam.id (Kernel.Types.Id.getId id),
      Se.Set Beam.totalRidesCount totalRidesCount
    ]
    [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]
