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

data ServiceTierType = ORDINARY | AC | NON_AC | EXPRESS | SPECIAL | EXECUTIVE | FIRST_CLASS | SECOND_CLASS | THIRD_CLASS
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToParamSchema)

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

data OrderStatus = SOFT_CANCELLED | CANCELLED | CANCEL_INITIATED | ACTIVE | COMPLETE
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON OrderStatus where
  parseJSON (String "SOFT_CANCEL") = pure SOFT_CANCELLED
  parseJSON (String "CANCELLED") = pure CANCELLED
  parseJSON (String "CANCEL_INITIATED") = pure CANCEL_INITIATED
  parseJSON (String "ACTIVE") = pure ACTIVE
  parseJSON (String "COMPLETE") = pure COMPLETE
  parseJSON (String _) = parseFail "Invalid OnCancel Order Status"
  parseJSON e = typeMismatch "String" e

instance ToJSON OrderStatus where
  toJSON SOFT_CANCELLED = String "SOFT_CANCEL"
  toJSON CANCELLED = String "CANCELLED"
  toJSON CANCEL_INITIATED = String "CANCEL_INITIATED"
  toJSON ACTIVE = String "ACTIVE"
  toJSON COMPLETE = String "COMPLETE"
