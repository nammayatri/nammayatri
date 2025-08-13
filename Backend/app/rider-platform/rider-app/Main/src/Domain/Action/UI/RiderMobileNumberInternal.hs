{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.RiderMobileNumberInternal (getRiderMobileNumber) where

import qualified API.Types.UI.RiderMobileNumberInternal
import Data.OpenApi (ToSchema)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Auth

getRiderMobileNumber :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.UI.RiderMobileNumberInternal.RiderMobileAPIEntity)
getRiderMobileNumber bppRideId mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  ride <- QRide.findByBPPRideId (Kernel.Types.Id.Id bppRideId) >>= fromMaybeM (InternalError "Ride not found")
  riderBooking <- QBooking.findById ride.bookingId >>= fromMaybeM (InternalError "Booking not found")
  riderDetails <- QPerson.findById riderBooking.riderId >>= fromMaybeM (InternalError "Rider not found")
  riderMobileNumber' <- traverse decrypt riderDetails.mobileNumber
  return $
    API.Types.UI.RiderMobileNumberInternal.RiderMobileAPIEntity
      { riderMobileNumber = riderMobileNumber'
      }
