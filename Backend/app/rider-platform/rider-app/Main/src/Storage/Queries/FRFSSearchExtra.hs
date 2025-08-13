module Storage.Queries.FRFSSearchExtra where

import Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam
import Storage.Queries.OrphanInstances.FRFSSearch ()

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create frfsSearchReq = create' frfsSearchReq

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

updateOnSearchFailed :: (MonadFlow m, EsqDBFlow m r) => Id FRFSSearch -> Maybe Bool -> m ()
updateOnSearchFailed (Id reqId) onSearchFailed = do
  updateOneWithKV
    [Se.Set Beam.onSearchFailed onSearchFailed]
    [Se.Is Beam.id (Se.Eq reqId)]
