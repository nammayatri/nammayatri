module Storage.Queries.CallStatusExtra where

import Domain.Types.CallStatus
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCS
import Storage.Queries.OrphanInstances.CallStatus ()

-- Extra code goes here --

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => CallStatus -> m ()
create cs = do
  callS <- findByCallSid (cs.callId)
  case callS of
    Nothing -> createWithKV cs
    Just _ -> pure ()

findByCallSid :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCS.callId $ Se.Eq callSid]

findOneByRideId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m (Maybe CallStatus)
findOneByRideId rideId = findAllWithOptionsKV [Se.Is BeamCS.rideId $ Se.Eq rideId] (Se.Desc BeamCS.createdAt) (Just 1) Nothing <&> listToMaybe
