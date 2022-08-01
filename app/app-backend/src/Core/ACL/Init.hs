module Core.ACL.Init (buildInitReq) where

import Beckn.Prelude
import Beckn.Types.App
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Init as Init
import Beckn.Types.Logging
import Beckn.Types.MapSearch (LatLong)
import Beckn.Utils.Context (buildTaxiContext)
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.VehicleVariant as VehVar
import qualified ExternalAPI.Flow as ExternalAPI

buildInitReq ::
  (ExternalAPI.HasBapInfo r m, MonadFlow m) =>
  DConfirm.ConfirmRes ->
  m (BecknReq Init.InitMessage)
buildInitReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.INIT res.bookingId.getId Nothing bapIDs.cabs bapURIs.cabs (Just res.providerId) (Just res.providerUrl)
  initMessage <- buildInitMessage res
  pure $ BecknReq context initMessage

buildInitMessage :: (MonadThrow m, Log m) => DConfirm.ConfirmRes -> m Init.InitMessage
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
      VehVar.AUTO -> Init.AUTO

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
