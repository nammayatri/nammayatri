{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FRFSQuoteExtra where

import Domain.Types.FRFSSearch
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuote as Beam

-- Extra code goes here --

updateManyRiderIdAndQuantityBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateManyRiderIdAndQuantityBySearchId riderId quantity searchId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.riderId (Kernel.Types.Id.getId riderId), Se.Set Beam.updatedAt _now, Se.Set Beam.quantity quantity] [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]
