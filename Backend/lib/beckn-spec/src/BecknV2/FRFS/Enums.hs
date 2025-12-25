{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module BecknV2.FRFS.Enums where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (constructorsToLowerOptions, constructorsWithHyphens)
import Kernel.Utils.TH

data Domain
  = FRFS
  deriving (Eq, Generic, Show, Read, FromDhall, ToSchema)
  deriving (PrettyShow) via Showable Domain

instance FromJSON Domain where
  parseJSON (String "ONDC:TRV11") = pure FRFS
  parseJSON (String _) = parseFail "Invalid Domain"
  parseJSON e = typeMismatch "String" e

instance ToJSON Domain where
  toJSON FRFS = String "ONDC:TRV11"

-- toJSON _ = error "Invalid Domain"

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | RATING
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_RATING
  | ON_SUPPORT
  deriving (Generic, Show, Eq, ToSchema)
  deriving (PrettyShow) via Showable Action

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

data VehicleCategory = METRO | SUBWAY | BUS
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''VehicleCategory)

data ServiceTierType = ORDINARY | AC | NON_AC | EXPRESS | SPECIAL | EXECUTIVE | FIRST_CLASS | SECOND_CLASS | THIRD_CLASS | ASHOK_LEYLAND_AC | MIDI_AC | VOLVO_AC | ELECTRIC_V | ELECTRIC_V_PMI | AC_EMU_FIRST_CLASS
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, ToParamSchema)

instance FromJSON ServiceTierType where
  parseJSON r = case r of
    (String "Deluxe EV") -> pure EXECUTIVE
    (String "Small Bus Express") -> pure EXPRESS
    (String "Small Bus Ordinary") -> pure NON_AC
    (String "A/C") -> pure AC
    (String "Ordinary") -> pure ORDINARY
    (String "A/C EV") -> pure AC
    (String "Express") -> pure EXPRESS
    (String "Deluxe") -> pure EXECUTIVE
    (String "ORDINARY") -> pure ORDINARY
    (String "AC") -> pure AC
    (String "NON_AC") -> pure NON_AC
    (String "EXPRESS") -> pure EXPRESS
    (String "SPECIAL") -> pure SPECIAL
    (String "EXECUTIVE") -> pure EXECUTIVE
    (String "FIRST_CLASS") -> pure FIRST_CLASS
    (String "SECOND_CLASS") -> pure SECOND_CLASS
    (String "THIRD_CLASS") -> pure THIRD_CLASS
    (String "ASHOK_LEYLAND_AC") -> pure ASHOK_LEYLAND_AC
    (String "MIDI_AC") -> pure MIDI_AC
    (String "VOLVO_AC") -> pure VOLVO_AC
    (String "ELECTRIC_V") -> pure ELECTRIC_V
    (String "ELECTRIC_V_PMI") -> pure ELECTRIC_V_PMI
    (String "AC_EMU_FIRST_CLASS") -> pure AC_EMU_FIRST_CLASS
    _ -> parseFail "Invalid Service Tier Type"

instance ToSchema ServiceTierType where
  declareNamedSchema proxy = do
    genericDeclareNamedSchema customSchemaOptions proxy
    where
      customSchemaOptions = defaultSchemaOptions {datatypeNameModifier = const "FRFSServiceTierType"}

$(mkHttpInstancesForEnum ''ServiceTierType)

data StopType = START | END | INTERMEDIATE_STOP
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data Network = BAP | BPP
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data PaymentType = PRE_ORDER | PRE_FULFILLMENT | ON_FULFILLMENT | POST_FULFILLMENT | ON_ORDER
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentStatus = PAID | NOT_PAID
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

data CancellationType = SOFT_CANCEL | CONFIRM_CANCEL
  deriving (Eq, Ord, Show, Read, Generic)

data CancellationParams = REFUND | CANCELLATION_CHARGES | BASE_FARE
  deriving (Eq, Ord, Show, Read, Generic)

data OrderStatus = SOFT_CANCELLED | CANCELLED | CANCEL_INITIATED | ACTIVE | COMPLETE | UPDATED
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON OrderStatus where
  parseJSON (String "SOFT_CANCEL") = pure SOFT_CANCELLED
  parseJSON (String "CANCELLED") = pure CANCELLED
  parseJSON (String "CANCEL_INITIATED") = pure CANCEL_INITIATED
  parseJSON (String "ACTIVE") = pure ACTIVE
  parseJSON (String "COMPLETE") = pure COMPLETE
  parseJSON (String "UPDATED") = pure UPDATED
  parseJSON (String _) = parseFail "Invalid Message Order Status"
  parseJSON e = typeMismatch "String" e

instance ToJSON OrderStatus where
  toJSON SOFT_CANCELLED = String "SOFT_CANCEL"
  toJSON CANCELLED = String "CANCELLED"
  toJSON CANCEL_INITIATED = String "CANCEL_INITIATED"
  toJSON ACTIVE = String "ACTIVE"
  toJSON COMPLETE = String "COMPLETE"
  toJSON UPDATED = String "UPDATED"
