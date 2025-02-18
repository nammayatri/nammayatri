{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationCharges where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CancellationCharges = CancellationCharges
  { cancellationCharges :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.CancellationCharges.CancellationCharges,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
  }
  deriving (Generic)
