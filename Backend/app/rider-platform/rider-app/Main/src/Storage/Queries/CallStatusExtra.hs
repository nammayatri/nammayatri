{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallStatusExtra where

import Domain.Types.CallStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCS
import Storage.Queries.OrphanInstances.CallStatus

-- Extra code goes here --

create :: KvDbFlow m r => CallStatus -> m ()
create cs = do
  callS <- findByCallSid (cs.callId)
  case callS of
    Nothing -> createWithKV cs
    Just _ -> pure ()

findByCallSid :: KvDbFlow m r => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCS.callId $ Se.Eq callSid]
