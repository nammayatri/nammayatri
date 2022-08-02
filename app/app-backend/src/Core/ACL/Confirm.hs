module Core.ACL.Confirm (buildConfirmReq) where

import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Beckn.Types.Id
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Booking.BookingLocation as DBL
import EulerHS.Prelude hiding (id)
import ExternalAPI.Flow
import Utils.Common

buildConfirmReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessage)
buildConfirmReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  context <- buildTaxiContext Context.CONFIRM messageId Nothing bapIDs.cabs bapURIs.cabs (Just res.bppId) (Just res.bppUrl)
  pure $ BecknReq context $ mkConfirmMessage res

mkConfirmMessage :: DOnInit.OnInitRes -> Confirm.ConfirmMessage
mkConfirmMessage res =
  Confirm.ConfirmMessage
    { order =
        Confirm.Order
          { id = getId res.bppBookingId,
            fulfillment = mkFulfillment res.fromLocationAddress res.mbToLocationAddress,
            customer =
              Confirm.OrderCustomer
                { contact =
                    Confirm.Contact
                      { phone =
                          Confirm.Phone
                            { country_code = res.riderPhoneCountryCode,
                              number = res.riderPhoneNumber
                            }
                      }
                },
            payment = mkPayment res.estimatedTotalFare
          }
    }

mkFulfillment :: DBL.LocationAddress -> Maybe DBL.LocationAddress -> Confirm.FulfillmentInfo
mkFulfillment startLoc mbStopLoc =
  Confirm.FulfillmentInfo
    { start =
        Confirm.StartInfo
          { location =
              Confirm.Location
                { address =
                    Confirm.Address
                      { area = startLoc.area,
                        state = startLoc.state,
                        country = startLoc.country,
                        building = startLoc.building,
                        door = startLoc.door,
                        street = startLoc.street,
                        city = startLoc.city,
                        area_code = startLoc.areaCode
                      }
                }
          },
      end =
        mbStopLoc <&> \stopLoc ->
          Confirm.StopInfo
            { location =
                Confirm.Location
                  { address =
                      Confirm.Address
                        { area = stopLoc.area,
                          state = stopLoc.state,
                          country = stopLoc.country,
                          building = stopLoc.building,
                          door = stopLoc.door,
                          street = stopLoc.street,
                          city = stopLoc.city,
                          area_code = stopLoc.areaCode
                        }
                  }
            }
    }

mkPayment :: Amount -> Confirm.Payment
mkPayment estimatedTotalFare =
  Confirm.Payment
    { collected_by = "BAP",
      params =
        Confirm.PaymentParams
          { amount = realToFrac estimatedTotalFare,
            currency = "INR"
          },
      time = Confirm.TimeDuration "P2D",
      _type = Confirm.ON_FULFILLMENT
    }
