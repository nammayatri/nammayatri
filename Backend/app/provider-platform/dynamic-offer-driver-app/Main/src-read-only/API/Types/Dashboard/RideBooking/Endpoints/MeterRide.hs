{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.MeterRide where

import qualified "this" API.Types.UI.PriceBreakup
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Ride
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("meterRide" :> GetMeterRidePrice)

type GetMeterRidePrice = ("price" :> MandatoryQueryParam "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> Get '[JSON] API.Types.UI.PriceBreakup.MeterRidePriceRes)

newtype MeterRideAPIs = MeterRideAPIs {getMeterRidePrice :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> EulerHS.Types.EulerClient API.Types.UI.PriceBreakup.MeterRidePriceRes}

mkMeterRideAPIs :: (Client EulerHS.Types.EulerClient API -> MeterRideAPIs)
mkMeterRideAPIs meterRideClient = (MeterRideAPIs {..})
  where
    getMeterRidePrice = meterRideClient

data MeterRideUserActionType
  = GET_METER_RIDE_PRICE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON MeterRideUserActionType where
  toJSON GET_METER_RIDE_PRICE = Data.Aeson.String "GET_METER_RIDE_PRICE"

instance FromJSON MeterRideUserActionType where
  parseJSON (Data.Aeson.String "GET_METER_RIDE_PRICE") = pure GET_METER_RIDE_PRICE
  parseJSON _ = fail "GET_METER_RIDE_PRICE expected"

$(Data.Singletons.TH.genSingletons [''MeterRideUserActionType])
