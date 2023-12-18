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
-- import Beckn.Types.Core.Taxi.Common.Tags
import Beckn.Types.Core.Taxi.Common.Time as Reexport
import Beckn.Types.Core.Taxi.Common.TimeDuration as Reexport
import Beckn.Types.Core.Taxi.Common.TimeTimestamp (TimeTimestamp)
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import EulerHS.Prelude hiding (State, id)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data PaymentV2 = PaymentV2
  { -- id :: Maybe Text, -- Probably will need to add this
    collectedBy :: PaymentCollector,
    uri :: Maybe Text,
    params :: PaymentParamsV2,
    _type :: PaymentType,
    status :: Maybe PaymentStatus,
    buyerAppFindeFeeType :: Maybe Text,
    buyerAppFinderFeeAmount :: Maybe DecimalValue,
    settlementDetails :: Maybe SettlementDetails
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON PaymentV2 where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON PaymentV2 where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data PaymentParamsV2 = PaymentParamsV2
  { currency :: Text,
    instrument :: Maybe PaymentInstrument,
    amount :: Maybe DecimalValue
    -- bankCode :: Maybe Text,
    -- bankAccountNumber :: Maybe Text,
    -- virtualPaymentAddress :: Maybe Text,
    -- sourceBankCode :: Maybe Text,
    -- sourceBankAccountNumber :: Maybe Text,
    -- sourceVirtualPaymentAddress :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema PaymentParamsV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON PaymentParamsV2 where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON PaymentParamsV2 where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data PaymentStatus = PAID | NOT_PAID
  deriving (Generic, Show)

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

instance ToSchema PaymentStatus where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

data SettlementDetails = SettlementDetails
  { settlementCounterparty :: Maybe Text,
    settlementPhase :: Maybe Text,
    settlementType :: Maybe Text,
    upiAddress :: Maybe Text,
    settlementBankAccountNo :: Maybe Text,
    settlementIfscCode :: Maybe Text,
    beneficiaryName :: Maybe Text,
    bankName :: Maybe Text,
    branchName :: Maybe Text,
    settlementReference :: Maybe Text,
    settlementStatus :: Maybe PaymentStatus,
    settlementTimestamp :: Maybe TimeTimestamp
  }
  deriving (Generic, Show)

instance FromJSON SettlementDetails where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON SettlementDetails where
  toJSON = genericToJSON constructorsWithHyphens

instance ToSchema SettlementDetails where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions constructorsWithHyphens

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Payment = Payment
  { params :: PaymentParams,
    _type :: PaymentType,
    uri :: Maybe Text
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
    amount :: Maybe DecimalValue
  }
  deriving (Generic, Show)

instance ToSchema PaymentParams where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON PaymentParams where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON PaymentParams where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
