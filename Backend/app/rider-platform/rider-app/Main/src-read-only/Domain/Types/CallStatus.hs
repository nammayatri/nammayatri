{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CallStatus where

import Data.Aeson
import qualified Domain.Types.Ride
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CallStatus = CallStatus
  { id :: Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus,
    callId :: Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    dtmfNumberUsed :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.External.Call.Interface.Types.CallStatus,
    recordingUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    conversationDuration :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    callService :: Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService,
    callError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
