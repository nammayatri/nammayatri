{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CRIS where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data CrisChangeDeviceRequest = CrisChangeDeviceRequest {otp :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetSDKDataRequest = GetSDKDataRequest {deviceID :: Data.Text.Text, mobileNo :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GetSDKDataResponse = GetSDKDataResponse {respCode :: Kernel.Prelude.Int, respMessage :: Data.Text.Text, sdkData :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
