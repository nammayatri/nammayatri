{-# LANGUAGE DerivingStrategies #-}

module Idfy.Types.Response where

import Beckn.Utils.JSON
import Beckn.Utils.Time
import Data.Aeson hiding (Error)
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import EulerHS.Prelude hiding (state)

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

-- class of vehicle
data ClassOfVehicle = W_NT | W_T | W_CAB | HGV_T | HMV_HGV | HMV | HTV | LMV | LMV_NT | LMV_T | LMV_CAB | LMV_HMV | LTV | MCWG | MCWOG | HPMV | MGV | MMV | LDRXCV | PSV_BUS | TRANS | TRCTOR | OTHERS | NotDefined Text
  deriving (Show, Generic, ToSchema, Eq, Read, ToJSON)

instance FromJSON ClassOfVehicle where
  parseJSON (String val) =
    case T.toUpper val of
      "3W-NT" -> return W_NT
      "3W-T" -> return W_T
      "3W-CAB" -> return W_CAB
      "HGV-T" -> return HGV_T
      "HMV-HGV" -> return HMV_HGV
      "HMV" -> return HMV
      "HTV" -> return HTV
      "LMV" -> return LMV
      "LMV-NT" -> return LMV_NT
      "LMV-T" -> return LMV_T
      "LMV-CAB" -> return LMV_CAB
      "LMV-HMV" -> return LMV_HMV
      "LTV" -> return LTV
      "MCWG" -> return MCWG
      "MCWOG" -> return MCWOG
      "HPMV" -> return HPMV
      "MGV" -> return MGV
      "MMV" -> return MMV
      "LDRXCV" -> return LDRXCV
      "PSV-BUS" -> return PSV_BUS
      "TRANS" -> return TRANS
      "TRCTOR" -> return TRCTOR
      "OTHERS" -> return OTHERS
      v -> return $ NotDefined v
  parseJSON _ = fail ""

constructorForCOVToJson :: Options
constructorForCOVToJson =
  defaultOptions
    { constructorTagModifier = \case
        "W_NT" -> "3W-NT"
        "W_T" -> "3W-T"
        "W_CAB" -> "3W-CAB"
        val -> T.unpack $ T.toLower $ replaceUnderscores $ T.pack val
    }

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
    vehicle_financier :: Maybe Text
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
    cov :: ClassOfVehicle,
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
    _type :: [ClassOfVehicle],
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
