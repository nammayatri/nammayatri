module Storage.Queries.TollExtra where

import qualified Domain.Types.Toll
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Toll as Beam

updateEnabledById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Toll.Toll ->
  Kernel.Prelude.Bool ->
  m ()
updateEnabledById tollId enabledVal = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabledVal,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId tollId)]
