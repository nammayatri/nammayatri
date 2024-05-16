{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverBlockReasonExtra where

import Data.Function
import Domain.Types.DriverBlockReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.DriverBlockReason as BeamDBR
import Storage.Queries.OrphanInstances.DriverBlockReason

-- Extra code goes here --

findAll :: KvDbFlow m r => m [DriverBlockReason]
findAll = findAllWithKV [Se.Is BeamDBR.reasonCode $ Se.Not $ Se.Eq ""]
