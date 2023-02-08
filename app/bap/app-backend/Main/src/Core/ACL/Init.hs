module Core.ACL.Init (buildInitReq) where

import qualified Beckn.Types.Core.Taxi.Init as Init
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.VehicleVariant as VehVar
import Environment
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Logging
import Kernel.Utils.Context (buildTaxiContext)

buildInitReq ::
  (HasBapInfo r m, MonadFlow m) =>
  DConfirm.DConfirmRes ->
  m (BecknReq Init.InitMessage)
buildInitReq res = do
  let transactionId = res.searchRequestId.getId
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.INIT res.booking.id.getId (Just transactionId) bapIDs.cabs bapURIs.cabs (Just res.providerId) (Just res.providerUrl)
  initMessage <- buildInitMessage res
  pure $ BecknReq context initMessage

buildInitMessage :: (MonadThrow m, Log m) => DConfirm.DConfirmRes -> m Init.InitMessage
buildInitMessage res = do
  let (fareProductType, mbDistance, mbDuration, mbBppItemId) = case res.quoteDetails of
        DConfirm.ConfirmOneWayDetails -> (Init.ONE_WAY_TRIP, Nothing, Nothing, Nothing)
        DConfirm.ConfirmRentalDetails r -> (Init.RENTAL_TRIP, Just r.baseDistance, Just r.baseDuration, Nothing)
        DConfirm.ConfirmAutoDetails bppQuoteId -> (Init.DRIVER_OFFER, Nothing, Nothing, Just bppQuoteId.getId)
  let vehicleVariant = castVehicleVariant res.vehicleVariant
  let itemCode =
        Init.ItemCode
          { fareProductType,
            vehicleVariant,
            distance = mbDistance,
            duration = mbDuration
          }
  pure
    Init.InitMessage
      { order =
          Init.Order
            { items = [mkOrderItem mbBppItemId itemCode],
              fulfillment = mkFulfillmentInfo res.fromLoc res.toLoc res.startTime,
              payment = mkPayment
            }
      }
  where
    castVehicleVariant = \case
      VehVar.SEDAN -> Init.SEDAN
      VehVar.SUV -> Init.SUV
      VehVar.HATCHBACK -> Init.HATCHBACK
      VehVar.AUTO_RICKSHAW -> Init.AUTO_RICKSHAW

mkOrderItem :: Maybe Text -> Init.ItemCode -> Init.OrderItem
mkOrderItem mbBppItemId code =
  Init.OrderItem
    { id = mbBppItemId,
      descriptor =
        Init.Descriptor
          { code = code
          }
    }

mkFulfillmentInfo :: LatLong -> Maybe LatLong -> UTCTime -> Init.FulfillmentInfo
mkFulfillmentInfo fromLoc mbToLoc startTime =
  Init.FulfillmentInfo
    { start =
        Init.StartInfo
          { location =
              Init.Location
                { gps =
                    Init.Gps
                      { lat = fromLoc.lat,
                        lon = fromLoc.lon
                      }
                },
            time = Init.TimeTimestamp startTime
          },
      end =
        mbToLoc >>= \toLoc ->
          Just
            Init.StopInfo
              { location =
                  Init.Location
                    { gps =
                        Init.Gps
                          { lat = toLoc.lat,
                            lon = toLoc.lon
                          }
                    }
              }
    }

mkPayment :: Init.Payment
mkPayment =
  Init.Payment
    { collected_by = "BAP",
      _type = Init.ON_FULFILLMENT,
      time = Init.TimeDuration "P2D"
    }
