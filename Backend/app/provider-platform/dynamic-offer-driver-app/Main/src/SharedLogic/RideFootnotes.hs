module SharedLogic.RideFootnotes
  ( RideFootnotesLogicInput (..),
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareParameters as DFareParams
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude

data RideFootnotesLogicInput = RideFootnotesLogicInput
  { ride :: DRide.Ride,
    booking :: DRB.Booking,
    rideFareParams :: Maybe DFareParams.FareParameters,
    bookingFareParams :: DFareParams.FareParameters,
    waitingTimeSeconds :: Maybe Int,
    waitingTimeWithUnits :: Maybe Text,
    freeWaitingTimeMinutes :: Maybe Int,
    extraDistanceWithUnits :: Maybe Text,
    language :: Text
  }
  deriving (Generic, Show, ToJSON)
