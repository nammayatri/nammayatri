{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Miscellaneous where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data QRScanTestReq = QRScanTestReq {base64Image :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data QRScanTestResp = QRScanTestResp {err :: Kernel.Prelude.Maybe Data.Text.Text, result :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
