{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WhiteListOrgExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as BeamBLO
import Storage.Queries.OrphanInstances.WhiteListOrg

-- Extra code goes here --

countTotalSubscribers :: KvDbFlow m r => m Int
countTotalSubscribers = findAllWithKV [Se.Is BeamBLO.id $ Se.Not $ Se.Eq ""] <&> length
