{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Utils.Types.VoipCallStatus where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Utils.Common.Voip.Types.VoipStorageType

data VoipCallStatus = VoipCallStatus
  { callId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    callStatus :: Utils.Common.Voip.Types.VoipStorageType.VoipStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Lib.Utils.Types.VoipCallStatus.VoipCallStatus,
    merchantCity :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Utils.Common.Voip.Types.VoipStorageType.Merchant,
    networkQuality :: Kernel.Prelude.Text,
    networkType :: Kernel.Prelude.Text,
    rideId :: Kernel.Types.Id.Id Utils.Common.Voip.Types.VoipStorageType.Ride,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
