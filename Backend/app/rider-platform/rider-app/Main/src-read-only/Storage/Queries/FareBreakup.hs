{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareBreakup (module Storage.Queries.FareBreakup, module ReExport) where

import qualified Domain.Types.FareBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.FareBreakupExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.FareBreakup.FareBreakup -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FareBreakup.FareBreakup] -> m ())
createMany = traverse_ create
