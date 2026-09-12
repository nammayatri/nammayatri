{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | BAP-side (rider-app) debug projections for the cross-platform ride-flow
-- debug endpoint. The endpoint reports both sides of a ride in one response, so
-- these types are needed by rider-app (which produces them) and by the provider
-- side (whose combined response embeds them). They live in lib-dashboard --
-- which both application servers depend on -- so neither app has to depend on
-- the other.
module Dashboard.Common.RideDebug where

import Data.OpenApi (ToSchema)
import Kernel.Prelude (FromJSON, Generic, ToJSON)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common

data BAPSideDebug = BAPSideDebug
  { searchRequest :: Kernel.Prelude.Maybe BAPSearchRequestDebug,
    estimates :: [BAPEstimateDebug],
    quotes :: [BAPQuoteDebug],
    driverOffers :: [BAPDriverOfferDebug],
    booking :: Kernel.Prelude.Maybe BAPBookingDebug,
    ride :: Kernel.Prelude.Maybe BAPRideDebug
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BAPSearchRequestDebug = BAPSearchRequestDebug
  { id :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    riderPreferredOption :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BAPEstimateDebug = BAPEstimateDebug
  { id :: Kernel.Prelude.Text,
    bppEstimateId :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    tripCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleServiceTier :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BAPQuoteDebug = BAPQuoteDebug
  { id :: Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    tripCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    quoteDetailsType :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BAPDriverOfferDebug = BAPDriverOfferDebug
  { id :: Kernel.Prelude.Text,
    bppQuoteId :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    durationToPickup :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BAPBookingDebug = BAPBookingDebug
  { id :: Kernel.Prelude.Text,
    bppBookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    estimatedTotalFare :: Kernel.Types.Common.HighPrecMoney,
    paymentUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BAPRideDebug = BAPRideDebug
  { id :: Kernel.Prelude.Text,
    bppRideId :: Kernel.Prelude.Text,
    shortId :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text,
    otp :: Kernel.Prelude.Text,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rideStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    trackingUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
