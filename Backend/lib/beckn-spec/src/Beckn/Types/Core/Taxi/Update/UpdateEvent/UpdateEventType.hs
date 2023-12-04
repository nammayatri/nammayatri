{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Update.UpdateEvent.UpdateEventType where

import Data.Aeson.Types
import Kernel.Prelude

data UpdateEventType
  = PAYMENT_COMPLETED
  | DESTINATION_CHANGED
  deriving (Show, Eq, Ord, Read, Generic, ToSchema)

-- Generic instances for type with single value will not work
instance FromJSON UpdateEventType where
  parseJSON (String "PAYMENT_COMPLETED") = pure PAYMENT_COMPLETED
  parseJSON (String "DESTINATION_CHANGED") = pure DESTINATION_CHANGED
  parseJSON (String _) = parseFail "Expected \"PAYMENT_COMPLETED\" or \"DESTINATION_CHANGED\""
  parseJSON e = typeMismatch "String" e

instance ToJSON UpdateEventType where
  toJSON = String . show
