{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FinanceInvoice where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data FinanceInvoicePdfResp = FinanceInvoicePdfResp {invoiceNumber :: Kernel.Prelude.Text, pdfBase64 :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
