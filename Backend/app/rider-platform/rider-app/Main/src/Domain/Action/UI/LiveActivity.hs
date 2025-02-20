module Domain.Action.UI.LiveActivity
  (
    liveActivityTokenResp,
    LiveActivityTokenReq
  )
where

import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Utils.Common
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import Storage.Beam.SchedulerJob ()
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Kernel.Prelude

data LiveActivityTokenReq = LiveActivityTokenReq
  { token :: String
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON LiveActivityTokenReq

instance ToJSON LiveActivityTokenReq

liveActivityTokenResp :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType, HasField "maxShards" r Int) => LiveActivityTokenReq -> m APISuccess.APISuccess
liveActivityTokenResp req = do
  logDebug $ "LiveActivityTokenReq: " <> show req
  logDebug $ "LiveActivityTokenReq: Fetching token" <> show req.token
  -- fetch P8 , keyid , teamid from the config
  -- encrypt the token using ES256
  
  pure APISuccess.Success

-- things to do 
  -- fetch the token 
  -- encrypt it using P8 , keyid , teamid using ES256
  -- store it into profile table
  -- make a request to firebase apns to send the notification to the user