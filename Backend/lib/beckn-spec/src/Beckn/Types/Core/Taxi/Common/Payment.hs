{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Payment
  ( module Beckn.Types.Core.Taxi.Common.Payment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentCollector as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentInstrument as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentType as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Payment = Payment
  { params :: Maybe PaymentParams,
    _type :: PaymentType,
    uri :: Maybe Text,
    tl_method :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Payment where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Payment where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data PaymentParams = PaymentParams
  { collected_by :: PaymentCollector,
    currency :: Text,
    instrument :: Maybe PaymentInstrument,
    amount :: Maybe DecimalValue,
    transaction_id :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema PaymentParams where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON PaymentParams where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON PaymentParams where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
