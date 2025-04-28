module Domain.Action.External.LiveEKD where

import API.External.LiveEKD as LiveEKD
import Domain.Types.External.LiveEKD as LiveEKD
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Utils.Common
import Tools.Metrics

liveEKDProdLoop :: (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["vocalyticsCnfg" ::: VocalyticsCnfg]) => Text -> Text -> Text -> m APISuccess
liveEKDProdLoop file call_id user_type = do
  vocalyticsCnfg <- asks (.vocalyticsCnfg)
  let url = vocalyticsCnfg.url
      token = Just vocalyticsCnfg.token
      req =
        LiveEKDRequest
          { file = file,
            call_id = call_id,
            user_type = user_type
          }
  prodLoopRes <- callAPI url (LiveEKD.liveEKD token req) "liveEKD" LiveEKD.liveEKDAPI >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_PROD_LOOP_LIVE_EKD_API") url)
  logDebug $ "prodLoopRes: " <> show prodLoopRes
  pure prodLoopRes
