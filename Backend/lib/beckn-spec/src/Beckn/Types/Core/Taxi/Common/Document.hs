{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Document where

import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- | ONDC:TRV10 order.documents[] entry — e.g. the INVOICE document shared in on_status.
data Document = Document
  { descriptor :: DocumentDescriptor,
    mime_type :: Maybe Text,
    url :: Text
  }
  deriving (Generic, Show)

instance ToSchema Document where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Document where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Document where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data DocumentDescriptor = DocumentDescriptor
  { code :: Text, -- e.g. "INVOICE"
    name :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema DocumentDescriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON DocumentDescriptor where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON DocumentDescriptor where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

-- | Build the ONDC INVOICE document entry from a (time-windowed, pre-signed) PDF URL.
--   Wire into the on_status / on_update order's documents[] once the ONDC path is pinned.
mkInvoiceDocuments :: Text -> [Document]
mkInvoiceDocuments docUrl =
  [ Document
      { descriptor =
          DocumentDescriptor
            { code = "INVOICE",
              name = Just "Invoice Document",
              short_desc = Just "Download your invoice document here.",
              long_desc = Nothing
            },
        mime_type = Just "application/pdf",
        url = docUrl
      }
  ]
