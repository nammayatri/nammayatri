module Idfy.Types.VerificationResult where

import Beckn.Utils.Time
import Data.Aeson hiding (Error)
import Data.OpenApi hiding (name)
import EulerHS.Prelude

type RCVerificationResponse = [RCVerificationValue]

data RCVerificationValue = RCVerificationValue
  { action :: Text,
    completed_at :: UTCTime,
    created_at :: UTCTime,
    group_id :: Text,
    request_id :: Text,
    result :: RCResult,
    status :: Text,
    task_id :: Text,
    _type :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DLVerificationResponse = DLVerificationResponse
  { action :: Text,
    completed_at :: UTCTime,
    created_at :: UTCTime,
    group_id :: Text,
    request_id :: Text,
    result :: DLResult,
    status :: Text,
    task_id :: Text,
    _type :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype RCResult = RCResult {extraction_output :: RCExtractionResult}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype DLResult = DLResult {source_output :: SourceOutput}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data RCExtractionResult = RCExtractionResult
  { avg_gross_vehicle_weight :: Maybe Float,
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
    permit_validity :: Maybe Text,
    permit_issue_date :: Maybe Text,
    permit_number :: Maybe Text,
    puc_number_upto :: Maybe Text,
    registration_date :: Maybe Text,
    registration_number :: Maybe Text,
    rto_name :: Maybe Text,
    status :: Maybe Text,
    vehicle_class :: Maybe ClassOfVehicle,
    vehicle_financier :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data SourceOutput = SourceOutput
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
    cov_details :: [CovDetail]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data CovDetail = CovDetail
  { category :: Text,
    cov :: ClassOfVehicle,
    issue_date :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ClassOfVehicle = W_NT | W_T | W_CAB | HGV_T | HMV_HGV | HMV | HTV | LMV | LMV_NT | LMV_T | LMV_CAB | LMV_HMV | LTV | MCWG | MCWOG | HPMV | MGV | MMV | LDRXCV | PSV_BUS | TRANS | TRCTOR | Others
  deriving (Show, Generic, ToSchema, Eq, Read)

instance FromJSON ClassOfVehicle where
  parseJSON = genericParseJSON constructorForCOVToJson

instance ToJSON ClassOfVehicle where
  toJSON = genericToJSON constructorForCOVToJson

constructorForCOVToJson :: Options
constructorForCOVToJson =
  defaultOptions
    { constructorTagModifier = \case
        "W_NT" -> "3W_NT"
        "W_T" -> "3W_T"
        "W_CAB" -> "3W_CAB"
        val -> val
    }
