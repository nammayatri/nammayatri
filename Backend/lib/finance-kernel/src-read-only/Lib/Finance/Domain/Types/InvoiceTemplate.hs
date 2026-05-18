{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.InvoiceTemplate where

import Data.Aeson
import qualified Domain.Types.Invoice
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data InvoiceTemplate = InvoiceTemplate
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate,
    invoiceType :: Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType,
    language :: Kernel.External.Types.Language,
    lineItemRowTemplate :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    template :: Kernel.Prelude.Text,
    totalsLineRowTemplate :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
