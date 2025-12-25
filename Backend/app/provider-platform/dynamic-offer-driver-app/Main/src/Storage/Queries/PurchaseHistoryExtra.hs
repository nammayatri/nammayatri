module Storage.Queries.PurchaseHistoryExtra where

import qualified Domain.Types.Person as SP
import Domain.Types.PurchaseHistory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PurchaseHistory as BeamDC
import Storage.Queries.OrphanInstances.PurchaseHistory ()

-- Extra code goes here --

getPurchasedHistory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> Maybe Integer -> Maybe Integer -> m [PurchaseHistory]
getPurchasedHistory (Id driverId) mbLimit mbOffset = do
  let limitVal = maybe 10 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
  findAllWithOptionsKV
    [Se.Is BeamDC.driverId $ Se.Eq driverId]
    (Se.Desc BeamDC.createdAt)
    (Just limitVal)
    (Just offsetVal)

createPurchaseHistory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PurchaseHistory -> m ()
createPurchaseHistory = createWithKV
