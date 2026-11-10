module Storage.Queries.RiderDriverCorrelationExtra where

import Domain.Types.Person (Person)
import Domain.Types.RiderDetails (RiderDetails)
import Domain.Types.RiderDriverCorrelation
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDriverCorrelation as Beam
import Storage.Queries.OrphanInstances.RiderDriverCorrelation ()

-- Extra code goes here --

-- | One rider's correlations with the given drivers. Redis is read once; Postgres is queried
-- only for the drivers with no correlation in Redis. (riderDetailId, driverId) is the primary key,
-- so a driver found in Redis is taken as complete.
-- Unlike 'Storage.Queries.RiderDriverCorrelation.findAllCorrelationsForRider' this does not
-- load the rider's correlations with every other driver.
findAllByRiderIdAndDriverIds ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id RiderDetails ->
  [Id Person] ->
  m [RiderDriverCorrelation]
findAllByRiderIdAndDriverIds riderDetailId driverIds =
  if null driverIds
    then pure []
    else findAllWithKVInBatch (clauseFor ids) Beam.driverId ids clauseFor
  where
    ids = getId <$> driverIds
    clauseFor driverIdTexts =
      [ Se.And
          [ Se.Is Beam.riderDetailId $ Se.Eq (getId riderDetailId),
            Se.Is Beam.driverId $ Se.In driverIdTexts
          ]
      ]
