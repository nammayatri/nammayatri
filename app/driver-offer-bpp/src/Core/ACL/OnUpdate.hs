module Core.ACL.OnUpdate
  ( buildOnUpdateMessage,
    OnUpdateBuildReq (..),
  )
where

import Beckn.Prelude
import Beckn.Types.Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as RideAssignedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent as BookingReallocationOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as RideCompletedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as RideStartedOU
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
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
        fareBreakups :: Text --[DFareBreakup.FareBreakup]
      }
  | BookingCancelledBuildReq
      { booking :: Text, --SRB.RideBooking,
        cancellationSource :: Text --SBCR.CancellationSource
      }
  | BookingReallocationBuildReq
      { booking :: Text, --SRB.RideBooking,
        rideId :: Id Text --SRide.Ride
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
          { id = "ride.bookingId.getId",
            update_target = "fufillment.state.code",
            fulfillment = RideStartedOU.FulfillmentInfo "ride.id.getId"
          }
buildOnUpdateMessage RideCompletedBuildReq {..} = do
  -- fare <- realToFrac <$> ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  -- totalFare <- realToFrac <$> ride.totalFare & fromMaybeM (InternalError "Total ride fare is not present.")
  -- chargeableDistance <- fmap realToFrac ride.chargeableDistance & fromMaybeM (InternalError "Chargeable ride distance is not present.")
  let price =
        RideCompletedOU.QuotePrice
          { currency = "INR",
            value = 1, --fare,
            computed_value = 1 --totalFare
          }
      breakup = [] -- mkFareBreakupItem <$> fareBreakups
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideCompleted
        RideCompletedOU.RideCompletedEvent
          { id = "ride.bookingId.getId",
            update_target = "fulfillment.state.code,quote.price,quote.breakup",
            quote =
              RideCompletedOU.RideCompletedQuote
                { ..
                },
            fulfillment =
              RideCompletedOU.FulfillmentInfo
                { id = "ride.id.getId",
                  chargeable_distance = 1 --chargeableDistance
                }
          }
-- where
--   mkFareBreakupItem :: DFareBreakup.FareBreakup -> RideCompletedOU.BreakupItem
--   mkFareBreakupItem DFareBreakup.FareBreakup {..} =
--     RideCompletedOU.BreakupItem
--       { title = description,
--         price =
--           RideCompletedOU.BreakupPrice
--             { currency = "INR",
--               value = realToFrac amount
--             }
--       }
buildOnUpdateMessage BookingCancelledBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideBookingCancelled
        BookingCancelledOU.RideBookingCancelledEvent
          { id = "booking.id.getId",
            state = "CANCELLED",
            update_target = "state,fufillment.state.code",
            cancellation_reason = BookingCancelledOU.ByUser --castCancellationSource cancellationSource
          }
buildOnUpdateMessage BookingReallocationBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideBookingReallocation
        BookingReallocationOU.RideBookingReallocationEvent
          { id = "booking.id.getId",
            update_target = "fulfillment.state.code",
            fulfillment = BookingReallocationOU.FulfillmentInfo rideId.getId
          }

-- castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
-- castCancellationSource = \case
--   SBCR.ByUser -> BookingCancelledOU.ByUser
--   SBCR.ByDriver -> BookingCancelledOU.ByDriver
--   SBCR.ByOrganization -> BookingCancelledOU.ByOrganization
--   SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
