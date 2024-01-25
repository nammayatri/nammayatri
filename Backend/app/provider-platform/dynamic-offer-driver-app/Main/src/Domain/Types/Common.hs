{-# LANGUAGE DeriveAnyClass #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.Common where

import qualified Data.List as List
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data UsageSafety = Safe | Unsafe

data TripCategory = OneWay OneWayMode | RoundTrip RoundTripMode | Rental RentalMode | RideShare RideShareMode
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, PrettyShow, ToJSON, ToSchema)

data TripOption = TripOption
  { schedule :: UTCTime,
    isScheduled :: Bool,
    tripCategories :: [TripCategory]
  }

data OneWayMode = OneWayRideOtp | OneWayOnDemandStaticOffer | OneWayOnDemandDynamicOffer
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable OneWayMode

type RoundTripMode = TripMode

type RentalMode = TripMode

type RideShareMode = TripMode

data TripMode = RideOtp | OnDemandStaticOffer
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TripMode

$(mkBeamInstancesForEnum ''TripCategory)

instance Show TripCategory where
  show (OneWay s) = "OneWay_" <> show s
  show (RoundTrip s) = "RoundTrip_" <> show s
  show (Rental s) = "Rental_" <> show s
  show (RideShare s) = "RideShare_" <> show s

instance Read TripCategory where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (OneWay v1, r2)
            | r1 <- stripPrefix "OneWay_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (RoundTrip v1, r2)
                 | r1 <- stripPrefix "RoundTrip_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (Rental v1, r2)
                 | r1 <- stripPrefix "Rental_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RideShare v1, r2)
                 | r1 <- stripPrefix "RideShare_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

isRideOtpBooking :: TripCategory -> Bool
isRideOtpBooking (OneWay OneWayRideOtp) = True
isRideOtpBooking (Rental RideOtp) = True
isRideOtpBooking (RoundTrip RideOtp) = True
isRideOtpBooking (RideShare RideOtp) = True
isRideOtpBooking _ = False

-- Move it to configs later if required
isEndOtpRequired :: TripCategory -> Bool
isEndOtpRequired (Rental _) = True
isEndOtpRequired _ = False

-- Move it to configs later if required
isOdometerReadingsRequired :: TripCategory -> Bool
isOdometerReadingsRequired (Rental _) = True
isOdometerReadingsRequired _ = False

-- Move it to configs later if required
isGoHomeAvailable :: TripCategory -> Bool
isGoHomeAvailable (OneWay _) = True
isGoHomeAvailable _ = False

isRentalTrip :: TripCategory -> Bool
isRentalTrip tripCategory = case tripCategory of
  Rental _ -> True
  _ -> False
