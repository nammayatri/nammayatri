{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Utils.Common.Voip.Types.VoipApiType where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
-- import qualified Kernel.Types.Id
import Servant
import qualified Tools.Beam.UtilsTH
import qualified Utils.Common.Voip.Types.VoipStorageType as VST

data UserType
  = DRIVER
  | RIDER
  deriving (Eq, Generic, Show, ToJSON, FromJSON, ToSchema)

-- error code definition can be found on this document: https://developer.clevertap.com/docs/signed-call-android-sdk
data VoipReq = VoipReq
  { callId :: Data.Text.Text,
    callStatus :: VST.VoipStatus,
    rideId :: Kernel.Types.Id.Id VST.Ride,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    userType :: UserType,
    networkType :: Data.Text.Text,
    networkQuality :: Data.Text.Text,
    merchantId :: Kernel.Types.Id.Id VST.Merchant,
    merchantCity :: Data.Text.Text
  }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, ToSchema)
