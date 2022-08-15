module Core.ACL.OnUpdate
  ( buildOnUpdateMessage,
    OnUpdateBuildReq (..),
  )
where

import Beckn.Prelude
import Beckn.Types.Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as RideAssignedOU
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as RideCompletedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as RideStartedOU
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FareParams as Fare
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Product.FareCalculator.Calculator (mkBreakupList)
import Types.Error
import Utils.Common

data OnUpdateBuildReq
  = RideAssignedBuildReq
      { driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        ride :: DRide.Ride
      }
  | RideStartedBuildReq
      { ride :: DRide.Ride
      }
  | RideCompletedBuildReq
      { ride :: DRide.Ride,
        fareParams :: Fare.FareParameters
      }
  | BookingCancelledBuildReq
      { booking :: SRB.Booking,
        cancellationSource :: SBCR.CancellationSource
      }

buildOnUpdateMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnUpdateBuildReq ->
  m OnUpdate.OnUpdateMessage
buildOnUpdateMessage RideAssignedBuildReq {..} = do
  mobileNumber <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
  name <- SP.getPersonFullName driver >>= fromMaybeM (PersonFieldNotPresent "firstName")
  let agent =
        RideAssignedOU.Agent
          { name = name,
            phone = mobileNumber,
            rating = realToFrac <$> driver.rating,
            tags = RideAssignedOU.AgentTags {registered_at = driver.createdAt}
          }
      veh =
        RideAssignedOU.Vehicle
          { model = vehicle.model,
            variant = show vehicle.variant,
            color = vehicle.color,
            registration = vehicle.registrationNo
          }
      fulfillment =
        RideAssignedOU.FulfillmentInfo
          { id = ride.id.getId,
            start =
              RideAssignedOU.StartInfo
                { authorization =
                    RideAssignedOU.Authorization
                      { _type = "OTP",
                        token = ride.otp
                      }
                },
            vehicle = veh,
            ..
          }
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideAssigned
        RideAssignedOU.RideAssignedEvent
          { id = ride.bookingId.getId,
            state = "ACTIVE",
            update_target = "state,fufillment.state.code,fulfillment.start.authorization,fulfillment.agent,fulfillment.vehicle",
            ..
          }
buildOnUpdateMessage RideStartedBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideStarted
        RideStartedOU.RideStartedEvent
          { id = ride.bookingId.getId,
            update_target = "fufillment.state.code",
            fulfillment = RideStartedOU.FulfillmentInfo ride.id.getId
          }
buildOnUpdateMessage req@RideCompletedBuildReq {} = do
  fare <- realToFrac <$> req.ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let currency = "INR"
      ride = req.ride
      price =
        RideCompletedOU.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup = mkBreakupList (OnUpdate.BreakupPrice currency . amountToDecimalValue) OnUpdate.BreakupItem req.fareParams
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideCompleted
        RideCompletedOU.RideCompletedEvent
          { id = ride.bookingId.getId,
            update_target = "fulfillment.state.code,quote.price,quote.breakup",
            quote =
              RideCompletedOU.RideCompletedQuote
                { price,
                  breakup
                },
            fulfillment =
              RideCompletedOU.FulfillmentInfo
                { id = ride.id.getId,
                  chargeable_distance = DecimalValue $ toRational ride.traveledDistance.getHighPrecMeters
                }
          }
buildOnUpdateMessage BookingCancelledBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.BookingCancelled
        BookingCancelledOU.BookingCancelledEvent
          { id = booking.id.getId,
            state = "CANCELLED",
            update_target = "state,fufillment.state.code",
            cancellation_reason = castCancellationSource cancellationSource
          }

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOU.ByUser
  SBCR.ByDriver -> BookingCancelledOU.ByDriver
  SBCR.ByOrganization -> BookingCancelledOU.ByOrganization
  SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
  SBCR.ByApplication -> BookingCancelledOU.ByApplication
