module Core.ACL.Init (buildInitReq) where

import Beckn.Prelude
import Beckn.Types.App
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Init as Init
import Beckn.Types.Field
import Beckn.Types.MapSearch (LatLong)
import Beckn.Utils.Context (buildTaxiContext)
import qualified Domain.Action.UI.Init as DInit
import qualified Domain.Types.Quote as Quote
import qualified Domain.Types.VehicleVariant as VehVar
import ExternalAPI.Flow

buildInitReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DInit.InitRes ->
  m (BecknReq Init.InitMessage)
buildInitReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.INIT res.messageId Nothing bapIDs.cabs bapURIs.cabs (Just res.providerId) (Just res.providerUrl)
  pure $ BecknReq context $ mkInitMessage res

mkInitMessage :: DInit.InitRes -> Init.InitMessage
mkInitMessage res =
  Init.InitMessage
    { order =
        Init.Order
          { items = [mkOrderItem itemCode],
            fulfillment = mkFulfillmentInfo res.fromLoc res.toLoc res.startTime,
            payment = mkPayment
          }
    }
  where
    itemCode = do
      let (fpType, mbDistance, mbDuration) = case res.quoteDetails of
            Quote.OneWayDetails _ -> (Init.ONE_WAY_TRIP, Nothing, Nothing)
            Quote.RentalDetails r -> (Init.RENTAL_TRIP, Just r.baseDistance, Just r.baseDuration)
          vehicleVariant = case res.vehicleVariant of
            VehVar.SEDAN -> Init.SEDAN
            VehVar.SUV -> Init.SUV
            VehVar.HATCHBACK -> Init.HATCHBACK
      Init.ItemCode fpType vehicleVariant mbDistance mbDuration

mkOrderItem :: Init.ItemCode -> Init.OrderItem
mkOrderItem code =
  Init.OrderItem
    { descriptor =
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
