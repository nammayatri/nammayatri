{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel (buildOnCancelMessage, OnCancelBuildReq (..)) where

import Beckn.ACL.Common (castCancellationSource)
import qualified Beckn.Types.Core.Taxi.Common.FulfillmentInfo as Fulfillment
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnCancel as OnCancel
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DV
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data OnCancelBuildReq = OnCancelBuildReq
  { ride :: DRide.Ride,
    booking :: DRB.Booking,
    estimateId :: Id DEst.Estimate,
    cancellationSource :: SBCR.CancellationSource,
    mbDriver :: Maybe DP.Person,
    mbVehicle :: Maybe DV.Vehicle,
    isRepeatSearch :: Bool
  }

mkFullfillment :: (EsqDBFlow m r, EncFlow m r) => DRide.Ride -> DRB.Booking -> Maybe Tags.TagGroups -> Maybe DP.Person -> Maybe DV.Vehicle -> m Fulfillment.FulfillmentInfo
mkFullfillment ride booking tags mbDriver mbVehicle = do
  let authorization =
        Fulfillment.Authorization
          { _type = "OTP",
            token = ride.otp
          }
  return $
    Fulfillment.FulfillmentInfo
      { id = ride.id.getId,
        start =
          Fulfillment.StartInfo
            { authorization =
                case booking.bookingType of
                  DRB.SpecialZoneBooking -> Just authorization
                  DRB.NormalBooking -> Just authorization, -- TODO :: Remove authorization for NormalBooking once Customer side code is decoupled.
              location =
                Fulfillment.Location
                  { gps = Fulfillment.Gps {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                  },
              time = Nothing
            },
        end =
          Fulfillment.EndInfo
            { location =
                Fulfillment.Location
                  { gps = Fulfillment.Gps {lat = booking.toLocation.lat, lon = booking.toLocation.lon} -- assuming locations will always be in correct order in list
                  },
              time = Nothing
            },
        agent =
          mbDriver <&> \driver ->
            OnCancel.Agent
              { name = driver.firstName,
                rateable = True,
                tags = Nothing,
                phone = driver.unencryptedMobileNumber,
                image = Nothing
              },
        _type = if booking.bookingType == DRB.NormalBooking then Fulfillment.RIDE else Fulfillment.RIDE_OTP,
        vehicle =
          mbVehicle <&> \vehicle ->
            Fulfillment.Vehicle
              { model = vehicle.model,
                variant = show vehicle.variant,
                color = vehicle.color,
                registration = vehicle.registrationNo
              },
        ..
      }

buildOnCancelMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnCancelBuildReq ->
  m OnCancel.OnCancelMessage
buildOnCancelMessage OnCancelBuildReq {..} = do
  let tagGroups =
        [ Tags.TagGroup
            { display = False,
              code = "cancellation_reasons",
              name = "Cancellation Reasons",
              list = [Tags.Tag (Just False) (Just "cancellation_reason") (Just "Cancellation Reason") (Just . show $ castCancellationSource cancellationSource)]
            },
          Tags.TagGroup
            { display = False,
              code = "estimate_info",
              name = "Estimate Info",
              list = [Tags.Tag (Just False) (Just "is_old_estimate_valid") (Just "Is Old Estimate Valid") (Just $ show isRepeatSearch)]
            }
        ]
  fulfillment <- mkFullfillment ride booking (Just $ Tags.TG tagGroups) mbDriver mbVehicle
  let item = OnCancel.Item {id = estimateId.getId}
  return $
    OnCancel.OnCancelMessage
      { order =
          OnCancel.Order
            { id = booking.id.getId,
              item,
              fulfillment
            }
      }
