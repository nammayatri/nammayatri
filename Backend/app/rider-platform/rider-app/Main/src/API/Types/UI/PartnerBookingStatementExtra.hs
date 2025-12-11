{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Types.UI.PartnerBookingStatementExtra
  ( module API.Types.UI.PartnerBookingStatement,
    module API.Types.UI.PartnerBookingStatementExtra,
  )
where

import API.Types.UI.PartnerBookingStatement
import qualified API.Types.UI.PartnerBookingStatement as PBSAPI
import Data.Aeson
-- import Kernel.Prelude
import Kernel.Utils.JSON (constructorsWithSnakeCase)

-- | Custom JSON instances with snake_case conversion for partner API compatibility
-- Uses constructorsWithSnakeCase from Kernel.Utils.JSON (mobility-core/shared-kernel)
-- Fields are defined in camelCase in Haskell but serialize to snake_case in JSON
-- These instances use OVERLAPPING to override the generated ones
instance ToJSON PBSAPI.BookingStatementReq where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.BookingStatementReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.BookingStatementRes where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.BookingStatementRes where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.BookingStatementItem where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.BookingStatementItem where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceDataReq where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceDataReq where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceDataRes where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceDataRes where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceItem where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceItem where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoicePerson where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoicePerson where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceUnitPricing where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceUnitPricing where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceGST where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceGST where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoicePaymentMode where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoicePaymentMode where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceBilling where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceBilling where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PBSAPI.InvoiceInvoice where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PBSAPI.InvoiceInvoice where
  parseJSON = genericParseJSON constructorsWithSnakeCase
