module ExternalBPP.CallAPI.Types where

import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified Tools.Metrics as Metrics
import qualified UrlShortner.Common as UrlShortner

type FRFSSearchFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r
  )

type FRFSConfirmFlow m r c =
  ( MonadFlow m,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasBAPMetrics m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool,
    HasShortDurationRetryCfg r c,
    HasLongDurationRetryCfg r c,
    SchedulerFlow r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasFlowEnv m r '["googleSAPrivateKey" ::: String]
  )
