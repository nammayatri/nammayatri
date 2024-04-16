{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CallStatus where

import Data.Aeson (eitherDecode)
import qualified Domain.Types.Ride
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CallStatus = CallStatus
  { callError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    callId :: Kernel.Prelude.Text,
    callService :: Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService,
    conversationDuration :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    dtmfNumberUsed :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recordingUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    status :: Kernel.External.Call.Interface.Types.CallStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
