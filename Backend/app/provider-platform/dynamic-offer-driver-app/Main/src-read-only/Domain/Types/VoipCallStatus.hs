{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VoipCallStatus where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Utils.Common.Voip.Types.VoipStorageType

data VoipCallStatus = VoipCallStatus
  { callId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    callStatus :: Utils.Common.Voip.Types.VoipStorageType.VoipStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.VoipCallStatus.VoipCallStatus,
    merchantCity :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    networkQuality :: Kernel.Prelude.Text,
    networkType :: Kernel.Prelude.Text,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
