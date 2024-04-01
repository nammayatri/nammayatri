{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Sos where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride
import qualified Domain.Types.Sos
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data SosReq = SosReq {flow :: Domain.Types.Sos.SosType, isRideEnded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SosRes = SosRes {sosId :: Kernel.Types.Id.Id Domain.Types.Sos.Sos} deriving (Generic, ToJSON, FromJSON, ToSchema)
