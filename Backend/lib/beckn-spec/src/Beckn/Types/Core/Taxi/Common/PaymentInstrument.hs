{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Beckn.Types.Core.Taxi.Common.PaymentInstrument where

import Data.Aeson.Types
import Data.OpenApi
import Kernel.Prelude

data PaymentInstrument = Card CardType | Wallet WalletType | UPI | NetBanking | Cash
  deriving (Generic, Show)

instance ToJSON PaymentInstrument where
  toJSON = genericToJSON paymentInstrumentOptions

instance FromJSON PaymentInstrument where
  parseJSON = genericParseJSON paymentInstrumentOptions

instance ToSchema PaymentInstrument where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions paymentInstrumentOptions

paymentInstrumentOptions :: Options
paymentInstrumentOptions =
  defaultOptions
    { sumEncoding = paymentInstrumentTaggedObject
    }

paymentInstrumentTaggedObject :: SumEncoding
paymentInstrumentTaggedObject =
  defaultTaggedObject
    { tagFieldName = "instrumentType",
      contentsFieldName = "instrumentName"
    }

-- TODO check schema
data CardType = DefaultCardType
  deriving (Generic, Show, ToSchema)

-- Generic instances for type with single value will not work
instance FromJSON CardType where
  parseJSON (String "DefaultCardType") = pure DefaultCardType
  parseJSON (String _) = parseFail "Expected \"DefaultCardType\""
  parseJSON e = typeMismatch "String" e

instance ToJSON CardType where
  toJSON = String . show

data WalletType = DefaultWalletType
  deriving (Generic, Show, ToSchema)

-- Generic instances for type with single value will not work
instance FromJSON WalletType where
  parseJSON (String "DefaultWalletType") = pure DefaultWalletType
  parseJSON (String _) = parseFail "Expected \"DefaultWalletType\""
  parseJSON e = typeMismatch "String" e

instance ToJSON WalletType where
  toJSON = String . show
