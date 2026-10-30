{-# LANGUAGE DeriveDataTypeable #-}

{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.RSF.Enums where

import Data.Aeson
import Data.Aeson.Types (parseFail, typeMismatch)
import Data.Data (Data)
import qualified Data.Text as T
import Kernel.Prelude

-- The text an enum value has on the wire (what its ToJSON writes), for storing it in a Text column.
rsfEnumText :: ToJSON a => a -> Text
rsfEnumText a = case toJSON a of
  String t -> t
  _ -> ""

data RSFDomain = NTS10
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON RSFDomain where
  parseJSON (String "ONDC:NTS10") = pure NTS10
  parseJSON (String _) = parseFail "Invalid RSF Domain: expected ONDC:NTS10"
  parseJSON e = typeMismatch "String" e

instance ToJSON RSFDomain where
  toJSON NTS10 = String "ONDC:NTS10"

data RSFAction
  = RECEIVER_RECON
  | ON_RECEIVER_RECON
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON RSFAction where
  parseJSON (String "receiver_recon") = pure RECEIVER_RECON
  parseJSON (String "on_receiver_recon") = pure ON_RECEIVER_RECON
  parseJSON (String s) = parseFail $ "Invalid RSF Action: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON RSFAction where
  toJSON RECEIVER_RECON = String "receiver_recon"
  toJSON ON_RECEIVER_RECON = String "on_receiver_recon"

data ReconStatus
  = RS_PROVISIONAL
  | RS_MATCHED
  | RS_DEEMED_SETTLED
  | RS_CLOSED
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON ReconStatus where
  parseJSON (String "01") = pure RS_PROVISIONAL
  parseJSON (String "02") = pure RS_MATCHED
  parseJSON (String "03") = pure RS_DEEMED_SETTLED
  parseJSON (String "04") = pure RS_CLOSED
  parseJSON (String s) = parseFail $ "Invalid recon_status: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON ReconStatus where
  toJSON RS_PROVISIONAL = String "01"
  toJSON RS_MATCHED = String "02"
  toJSON RS_DEEMED_SETTLED = String "03"
  toJSON RS_CLOSED = String "04"

data OrderReconStatus
  = ORS_PROVISIONAL
  | ORS_FINALE
  | ORS_OVERDUE
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON OrderReconStatus where
  parseJSON (String "01") = pure ORS_PROVISIONAL
  parseJSON (String "02") = pure ORS_FINALE
  parseJSON (String "03") = pure ORS_OVERDUE
  parseJSON (String s) = parseFail $ "Invalid order_recon_status: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON OrderReconStatus where
  toJSON ORS_PROVISIONAL = String "01"
  toJSON ORS_FINALE = String "02"
  toJSON ORS_OVERDUE = String "03"

data CounterpartyReconStatus
  = CRS_PAID
  | CRS_OVERPAID
  | CRS_UNDERPAID
  | CRS_NOT_PAID
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON CounterpartyReconStatus where
  parseJSON (String "01") = pure CRS_PAID
  parseJSON (String "02") = pure CRS_OVERPAID
  parseJSON (String "03") = pure CRS_UNDERPAID
  parseJSON (String "04") = pure CRS_NOT_PAID
  parseJSON (String s) = parseFail $ "Invalid counterparty_recon_status: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON CounterpartyReconStatus where
  toJSON CRS_PAID = String "01"
  toJSON CRS_OVERPAID = String "02"
  toJSON CRS_UNDERPAID = String "03"
  toJSON CRS_NOT_PAID = String "04"

data SettlementReasonCode
  = SRC_ORDER_PAYMENT
  | SRC_PART_REFUND
  | SRC_REFUND
  | SRC_CORRECTION
  | SRC_BUYER_APP_FEE
  | SRC_BUYER_APP_FEE_GST
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON SettlementReasonCode where
  parseJSON (String "01") = pure SRC_ORDER_PAYMENT
  parseJSON (String "02") = pure SRC_PART_REFUND
  parseJSON (String "03") = pure SRC_REFUND
  parseJSON (String "04") = pure SRC_CORRECTION
  parseJSON (String "05") = pure SRC_BUYER_APP_FEE
  parseJSON (String "06") = pure SRC_BUYER_APP_FEE_GST
  parseJSON (String s) = parseFail $ "Invalid settlement_reason_code: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON SettlementReasonCode where
  toJSON SRC_ORDER_PAYMENT = String "01"
  toJSON SRC_PART_REFUND = String "02"
  toJSON SRC_REFUND = String "03"
  toJSON SRC_CORRECTION = String "04"
  toJSON SRC_BUYER_APP_FEE = String "05"
  toJSON SRC_BUYER_APP_FEE_GST = String "06"

data WireSettlementStatus
  = WSS_PAID
  | WSS_NOT_PAID
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON WireSettlementStatus where
  parseJSON (String "PAID") = pure WSS_PAID
  parseJSON (String "NOT_PAID") = pure WSS_NOT_PAID
  parseJSON (String "NOT-PAID") = pure WSS_NOT_PAID
  parseJSON (String s) = parseFail $ "Invalid settlement_status: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON WireSettlementStatus where
  toJSON WSS_PAID = String "PAID"
  toJSON WSS_NOT_PAID = String "NOT_PAID"

-- on_receiver_recon orders[].message.code: an amount deviation (less / more / missing) or an RSF error code (70000-70021).
data DiffMessageCode
  = DMC_LESS
  | DMC_MORE
  | DMC_MISSING
  | DMC_ERROR Text
  deriving (Eq, Ord, Generic, Show, Read, Data)

instance FromJSON DiffMessageCode where
  parseJSON (String "less") = pure DMC_LESS
  parseJSON (String "more") = pure DMC_MORE
  parseJSON (String "missing") = pure DMC_MISSING
  parseJSON (String s)
    | T.length s == 5 && "700" `T.isPrefixOf` s && T.all (`elem` ['0' .. '9']) s = pure (DMC_ERROR s)
    | otherwise = parseFail $ "Invalid message.code: " <> show s
  parseJSON e = typeMismatch "String" e

instance ToJSON DiffMessageCode where
  toJSON DMC_LESS = String "less"
  toJSON DMC_MORE = String "more"
  toJSON DMC_MISSING = String "missing"
  toJSON (DMC_ERROR code) = String code
