module Storage.Queries.DriverBlockReasonExtra where

import Data.Function
import Domain.Types.DriverBlockReason
import Kernel.Beam.Functions
import Kernel.Types.Common
import Kernel.Utils.Common (CacheFlow)
import Sequelize as Se
import qualified Storage.Beam.DriverBlockReason as BeamDBR
import Storage.Queries.OrphanInstances.DriverBlockReason ()

-- Extra code goes here --

findAll :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [DriverBlockReason]
findAll = findAllWithKV [Se.Is BeamDBR.reasonCode $ Se.Not $ Se.Eq ""]
