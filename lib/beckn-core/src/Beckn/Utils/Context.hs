module Beckn.Utils.Context where

import Beckn.Types.App
import qualified Beckn.Types.Core.Migration1.Common.Context as Mig1
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import EulerHS.Prelude

buildMobilityContext1 ::
  (MonadTime m, MonadGuid m) =>
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  m Mig1.Context
buildMobilityContext1 txnId bapUri bppUri = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    Mig1.Context
      { domain = Mig1.MOBILITY,
        core_version = "0.9.3",
        bap_id = Just "",
        bap_uri = bapUri,
        bpp_id = Just "",
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = msgId,
        timestamp = currTime
      }

updateContext1 :: (MonadTime m, MonadGuid m) => Mig1.Context -> m Mig1.Context
updateContext1 context = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    context & #timestamp .~ currTime
      & #message_id .~ msgId
