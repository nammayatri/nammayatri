 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DerivingStrategies #-}

module Idfy.Types.Response where

import Data.Aeson hiding (Error)
import Data.OpenApi hiding (name)
import EulerHS.Prelude hiding (state)
import Kernel.Utils.JSON
import Kernel.Utils.Time

type ImageValidateResponse = IdfyResponse ValidateResponse

type RCExtractResponse = IdfyResponse (ExtractionOutput RCExtractionOutput)

type DLExtractResponse = IdfyResponse (ExtractionOutput DLExtractionOutput)

type VerificationResponse = IdfyResponse IdfyResult

type VerificationResponseList = [IdfyResponse IdfyResult]

type IdfyResult = Output DLVerificationOutput RCVerificationOutput

data IdfyResponse a = IdfyResponse
  { action :: Text,
    completed_at :: UTCTime,
    created_at :: UTCTime,
    group_id :: Text,
    request_id :: Text,
    result :: Maybe a,
    status :: Text,
    task_id :: Text,
    _type :: Text
  }
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (IdfyResponse a) where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance (FromJSON a) => FromJSON (IdfyResponse a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (IdfyResponse a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data IdfySuccess = IdfySuccess {request_id :: Text, _a :: Maybe Text}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- RC Result
newtype ExtractionOutput a = ExtractionOutput {extraction_output :: a}
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (ExtractionOutput a)

instance (ToJSON a) => ToJSON (ExtractionOutput a)

instance (FromJSON a) => FromJSON (ExtractionOutput a)

-- DL Result
newtype SourceOutput a = SourceOutput {source_output :: a}
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (SourceOutput a)

instance (ToJSON a) => ToJSON (SourceOutput a)

instance (FromJSON a) => FromJSON (SourceOutput a)

data Output a b = Output {source_output :: Maybe a, extraction_output :: Maybe b}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- RC verification response
data RCVerificationOutput = RCVerificationOutput
  { avg_gross_vehicle_weight :: Maybe Text,
    axle_configuration :: Maybe Text,
    chassis_number :: Maybe Text,
    emission_norms :: Maybe Text,
    color :: Maybe Text,
    colour :: Maybe Text,
    engine_number :: Maybe Text,
    fitness_upto :: Maybe Text,
    fuel_type :: Maybe Text,
    insurance_details :: Maybe Text,
    insurance_validity :: Maybe Text,
    manufacturer :: Maybe Text,
    mv_tax_upto :: Maybe Text,
    owner_name :: Maybe Text,
    permit_type :: Maybe Text,
    permit_validity_upto :: Maybe Text,
    permit_issue_date :: Maybe Text,
    permit_number :: Maybe Text,
    puc_validity_upto :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_name :: Maybe Text,
    status :: Maybe Text,
    vehicle_class :: Maybe Text,
    vehicle_financier :: Maybe Text,
    noc_valid_upto :: Maybe Text,
    seating_capacity :: Maybe Text,
    variant :: Maybe Text,
    npermit_upto :: Maybe Text,
    manufacturer_model :: Maybe Text,
    standing_capacity :: Maybe Text,
    status_message :: Maybe Text,
    number_of_cylinder :: Maybe Text,
    colour :: Maybe Text,
    puc_valid_upto :: Maybe Text,
    permanent_address :: Maybe Text,
    permit_no :: Maybe Text,
    father_name :: Maybe Text,
    status_verfy_date :: Maybe Text,
    m_y_manufacturing :: Maybe Text,
    gross_vehicle_weight :: Maybe Text,
    registered_place :: Maybe Text,
    insurance_policy_no :: Maybe Text,
    noc_details :: Maybe Text,
    npermit_issued_by :: Maybe Text,
    sleeper_capacity :: Maybe Text,
    current_address :: Maybe Text,
    status_verification :: Maybe Text,
    permit_validity_from :: Maybe Text,
    puc_number :: Maybe Text,
    owner_mobile_no :: Maybe Text,
    blacklist_status :: Maybe Text,
    body_type :: Maybe Text,
    unladden_weight :: Maybe Text,
    insurance_name :: Maybe Text,
    owner_serial_number :: Maybe Text,
    vehicle_category :: Maybe Text,
    npermit_no :: Maybe Text,
    cubic_capacity :: Maybe Text,
    norms_type :: Maybe Text,
    financer :: Maybe Text,
    wheelbase :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- DL Verification response
data DLVerificationOutput = DLVerificationOutput
  { address :: Maybe Text,
    badge_details :: Maybe Text,
    card_serial_no :: Maybe Text,
    city :: Maybe Text,
    date_of_issue :: Maybe Text,
    date_of_last_transaction :: Maybe Text,
    dl_status :: Maybe Text,
    dob :: Maybe Text,
    face_image :: Maybe Text,
    gender :: Maybe Text,
    hazardous_valid_till :: Maybe Text,
    hill_valid_till :: Maybe Text,
    id_number :: Maybe Text,
    issuing_rto_name :: Maybe Text,
    last_transacted_at :: Maybe Text,
    name :: Maybe Text,
    nt_validity_from :: Maybe Text,
    nt_validity_to :: Maybe Text,
    relatives_name :: Maybe Text,
    source :: Maybe Text,
    status :: Maybe Text,
    t_validity_from :: Maybe Text,
    t_validity_to :: Maybe Text,
    cov_details :: Maybe [CovDetail]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data CovDetail = CovDetail
  { category :: Maybe Text,
    cov :: Text,
    issue_date :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- validate image
data ValidateResponse = ValidateResponse
  { detected_doc_type :: Text,
    is_readable :: Maybe Bool,
    readability :: ReadabilityBody
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ReadabilityBody = ReadabilityBody
  { confidence :: Maybe Int,
    dummyField :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- DL extract response
data DLExtractionOutput = DLExtractionOutput
  { id_number :: Maybe Text,
    name_on_card :: Maybe Text,
    fathers_name :: Maybe Text,
    date_of_birth :: Maybe Text,
    date_of_validity :: Maybe Text,
    address :: Maybe Text,
    district :: Maybe Text,
    street_address :: Maybe Text,
    pincode :: Maybe Text,
    state :: Maybe Text,
    issue_dates :: Maybe ValidateIssueDate,
    _type :: [Text],
    validity :: Maybe Validity,
    status :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema DLExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON DLExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DLExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Validity = Validity
  { nt :: Maybe Text,
    t :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema Validity where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON Validity where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON Validity where
  toJSON = genericToJSON constructorsToUpperOptions

data ValidateIssueDate = ValidateIssueDate
  { lmv :: Maybe Text,
    mcwg :: Maybe Text,
    trans :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema ValidateIssueDate where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON ValidateIssueDate where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON ValidateIssueDate where
  toJSON = genericToJSON constructorsToUpperOptions

-- RC Extraction
data RCExtractionOutput = RCExtractionOutput
  { address :: Maybe Text,
    body :: Maybe Text,
    chassis_number :: Maybe Text,
    _class :: Maybe Text,
    colour :: Maybe Text,
    cubic_capacity :: Maybe Text,
    document1_side :: Maybe Text,
    document2_side :: Maybe Text,
    engine_number :: Maybe Text,
    fathers_name :: Maybe Text,
    fuel :: Maybe Text,
    manufacturer :: Maybe Text,
    manufacturing_date :: Maybe Text,
    model :: Maybe Text,
    owner_name :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_district :: Maybe Text,
    state :: Maybe Text,
    wheel_base :: Maybe Text,
    status :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema RCExtractionOutput where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON RCExtractionOutput where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON RCExtractionOutput where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
