{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.PaymentType where

import Data.OpenApi
import Kernel.Prelude
import Kernel.Utils.JSON (constructorsWithHyphens)
import Kernel.Utils.Schema

data PaymentType
  = ON_ORDER
  | ON_FULFILLMENT
  | PRE_ORDER
  | PRE_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show)

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance ToSchema PaymentType where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens
