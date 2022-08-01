module Core.ACL.Select (buildSelectReq) where

import Beckn.Prelude
import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Domain.Action.UI.Select as DSelect
import ExternalAPI.Flow
import Utils.Common

buildSelectReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSelect.DSelectReq ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectReq = do
  let messageId = dSelectReq.estimateId.getId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SELECT messageId Nothing bapIDs.cabs bapURIs.cabs (Just dSelectReq.providerId) (Just dSelectReq.providerUrl)
  let order = mkIntent dSelectReq
  pure $ BecknReq context $ Select.SelectMessage order

mkIntent :: DSelect.DSelectReq -> Select.Intent
mkIntent req = do
  let from = req.fromLocation
      mbTo = req.toLocation
  Select.Intent
    { fulfillment =
        Select.FulfillmentInfo
          { start =
              Select.StartInfo
                { location = Select.Location Select.Gps {lat = from.lat, lon = from.lon},
                  time = Select.TimeTimestamp req.searchRequest.startTime
                },
            end =
              mbTo <&> \to ->
                Select.StopInfo
                  { location = Select.Location Select.Gps {lat = to.lat, lon = to.lon}
                  }
          }
    }
