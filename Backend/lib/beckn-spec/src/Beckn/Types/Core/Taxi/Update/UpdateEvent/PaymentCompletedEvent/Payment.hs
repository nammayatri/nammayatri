{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent.Payment
  ( module Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent.Payment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentCollector as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentInstrument as Reexport
import Beckn.Types.Core.Taxi.Common.PaymentType as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import Kernel.Prelude
import Kernel.Utils.JSON as JSON
import Kernel.Utils.Schema

data PaymentV2 = PaymentV2
  { collected_by :: PaymentCollector,
    _type :: PaymentType,
    status :: PaymentStatus,
    params :: PaymentParamsV2
  }
  deriving (Generic, Show)

instance FromJSON PaymentV2 where
  parseJSON = genericParseJSON JSON.stripPrefixUnderscoreIfAny

instance ToJSON PaymentV2 where
  toJSON = genericToJSON JSON.stripPrefixUnderscoreIfAny

instance ToSchema PaymentV2 where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions JSON.stripPrefixUnderscoreIfAny

newtype PaymentParamsV2 = PaymentParamsV2
  { instrument :: PaymentInstrument
  }
  deriving (Generic, Show)

instance ToSchema PaymentParamsV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON PaymentParamsV2 where
  parseJSON = genericParseJSON JSON.stripPrefixUnderscoreIfAny

instance ToJSON PaymentParamsV2 where
  toJSON = genericToJSON JSON.stripPrefixUnderscoreIfAny

data PaymentStatus = PAID | NOT_PAID
  deriving (Generic, Show)

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

instance ToSchema PaymentStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Payment = Payment
  { collected_by :: PaymentCollector,
    _type :: PaymentType,
    instrument :: PaymentInstrument, -- FIXME find proper fields
    status :: PaymentStatus
  }
  deriving (Generic, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON JSON.stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON JSON.stripPrefixUnderscoreIfAny

instance ToSchema Payment where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions JSON.stripPrefixUnderscoreIfAny
