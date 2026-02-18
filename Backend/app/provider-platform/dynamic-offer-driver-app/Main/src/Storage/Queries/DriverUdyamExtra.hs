module Storage.Queries.DriverUdyamExtra where

import Domain.Types.DriverUdyam
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverUdyam as Beam
import Storage.Queries.DriverUdyam ()

findAllByEncryptedUdyamNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m [DriverUdyam]
findAllByEncryptedUdyamNumber udyamNumberHash = do
  findAllWithKV [Se.Is Beam.udyamNumberHash $ Se.Eq udyamNumberHash]
