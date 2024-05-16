{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RentalDetailsExtra where

import Domain.Types.RentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.OrphanInstances.RentalDetails

-- Extra code goes here --
createRentalDetails :: KvDbFlow m r => RentalDetails -> m ()
createRentalDetails = createWithKV
