{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CallStatus where

import Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CallStatus = CallStatus
  { callAttempt :: Kernel.Prelude.Maybe Domain.Types.CallStatus.CallAttemptStatus,
    callError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    callId :: Kernel.Prelude.Text,
    callService :: Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService,
    conversationDuration :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    dtmfNumberUsed :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    recordingUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.External.Call.Interface.Types.CallStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CallAttemptStatus = Attempted | Resolved | Failed | Pending deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CallAttemptStatus))
