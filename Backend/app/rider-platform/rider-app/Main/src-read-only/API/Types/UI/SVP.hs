{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SVP where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.SvpJourney
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data GateCallbackReq = GateCallbackReq
  { merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mobileNumber :: Data.Text.Text,
    scanType :: ScanType,
    stationCode :: Data.Text.Text,
    timestamp :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GateCallbackResp = GateCallbackResp
  { allowed :: Kernel.Prelude.Bool,
    fareCharged :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    reason :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GenerateQrResp = GenerateQrResp {journeyStatus :: Kernel.Prelude.Maybe Domain.Types.SvpJourney.SvpJourneyStatus, qrData :: Data.Text.Text, tktSlNo :: Data.Text.Text, validTill :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScanType
  = ENTRY
  | EXIT
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SignQRReq = SignQRReq {plaintext :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SignQRResp = SignQRResp {signature :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
