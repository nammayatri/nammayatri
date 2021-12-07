module Beckn.Utils.Context where

import Beckn.Types.App
import qualified Beckn.Types.Core.Cabs.Common.Context as Cab
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import EulerHS.Prelude

buildCabsContext ::
  (MonadTime m, MonadGuid m) =>
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  m Cab.Context
buildCabsContext txnId bapUri bppUri = do
  currTime <- getCurrentTime
  msgId <- generateGUIDText
  return $
    Cab.Context
      { domain = Cab.MOBILITY,
        core_version = "0.9.3",
        bap_id = Just "",
        bap_uri = bapUri,
        bpp_id = Just "",
        bpp_uri = bppUri,
        transaction_id = txnId,
        message_id = msgId,
        timestamp = currTime
      }