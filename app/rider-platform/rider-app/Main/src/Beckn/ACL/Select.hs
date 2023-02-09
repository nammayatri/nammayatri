module Beckn.ACL.Select (buildSelectReq) where

import qualified Beckn.Types.Core.Taxi.Common.ItemCode as Common
import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Domain.Action.UI.Select as DSelect
import Domain.Types.VehicleVariant
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Utils.Common

buildSelectReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DSelect.DSelectRes ->
  m (BecknReq Select.SelectMessage)
buildSelectReq dSelectReq = do
  let messageId = dSelectReq.estimateId.getId
  let transactionId = dSelectReq.searchRequest.id.getId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.SELECT messageId (Just transactionId) bapIDs.cabs bapURIs.cabs (Just dSelectReq.providerId) (Just dSelectReq.providerUrl)
  let order = mkOrder dSelectReq
  pure $ BecknReq context $ Select.SelectMessage order

castVariant :: VehicleVariant -> Common.VehicleVariant
castVariant AUTO_RICKSHAW = Common.AUTO_RICKSHAW
castVariant HATCHBACK = Common.HATCHBACK
castVariant SEDAN = Common.SEDAN
castVariant SUV = Common.SUV

mkOrder :: DSelect.DSelectRes -> Select.Order
mkOrder req = do
  let from = req.searchRequest.fromLocation
      mbTo = req.searchRequest.toLocation
      items =
        (: []) $
          Select.OrderItem
            { id = Nothing,
              descriptor =
                Select.Descriptor
                  { code =
                      Select.ItemCode
                        { fareProductType = Common.DRIVER_OFFER_ESTIMATE,
                          vehicleVariant = castVariant req.variant,
                          distance = Nothing,
                          duration = Nothing
                        }
                  }
            }
  Select.Order
    { items,
      fulfillment =
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
