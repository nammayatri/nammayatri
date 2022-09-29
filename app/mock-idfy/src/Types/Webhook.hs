module Types.Webhook where

import Beckn.Utils.Time
import Data.OpenApi hiding (name)
import EulerHS.Prelude
import Types.API.VerifyDLAsync
import Types.API.VerifyRCAsync

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
    vehicle_class :: Maybe Text,
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
  { category :: Maybe Text,
    cov :: Maybe Text,
    issue_date :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

mkSuccessRC :: VerifyRCReq -> Text -> UTCTime -> RCVerificationResponse
mkSuccessRC VerifyRCReq {..} request_id now =
  [ RCVerificationValue
      { action = "verify_with_source",
        completed_at = now,
        created_at = now,
        group_id = group_id,
        request_id = request_id,
        status = "completed",
        task_id = task_id,
        _type = "ind_rc_basic",
        result =
          RCResult
            { extraction_output =
                RCExtractionResult
                  { avg_gross_vehicle_weight = Nothing,
                    axle_configuration = Nothing,
                    chassis_number = Just "MB8DP12DMM89XXXXX",
                    emission_norms = Just "BHARAT STAGE VI",
                    engine_number = Just "AF2127XXXXX",
                    fitness_upto = Just "2036-12-27",
                    fuel_type = Just "PETROL",
                    insurance_details = Nothing,
                    insurance_validity = Just "3026-12-21",
                    manufacturer = Just "SUZUKI MOTORCYCLE INDIA PVT LTD",
                    mv_tax_upto = Nothing,
                    owner_name = Just "PRITHIVI RAJ KOTA",
                    permit_issue_date = Just "1900-01-01",
                    permit_number = Just "",
                    permit_type = Nothing,
                    permit_validity = Just "3000-01-01",
                    puc_number_upto = Just "1900-01-01",
                    registration_date = Just "2021-12-28",
                    registration_number = Just "KA01JN9280",
                    rto_name = Nothing,
                    status = Just "id_found",
                    vehicle_class = Just "M-Cycle/Scooter(2WN)",
                    vehicle_financier = Nothing
                  }
            }
      }
  ]

mkSuccessDL :: VerifyDLReq -> Text -> UTCTime -> DLVerificationResponse
mkSuccessDL VerifyDLReq {..} request_id now =
  DLVerificationResponse
    { action = "verify_with_source",
      completed_at = now,
      created_at = now,
      group_id = group_id,
      request_id = request_id,
      status = "completed",
      task_id = task_id,
      _type = "ind_driving_license",
      result =
        DLResult
          { source_output =
              SourceOutput
                { address = Just "Address as available on Gov. Source",
                  badge_details = Just "Badge details",
                  card_serial_no = Just "Card serial no",
                  city = Just "ABC",
                  cov_details =
                    [ CovDetail
                        { category = Just "NT",
                          cov = Just "MCWG",
                          issue_date = Just "2015-05-20"
                        },
                      CovDetail
                        { category = Just "NT",
                          cov = Just "LMV",
                          issue_date = Just "2015-05-20"
                        }
                    ],
                  date_of_issue = Just "2015-05-20",
                  date_of_last_transaction = Nothing,
                  dl_status = Just "Active",
                  dob = Just "1985-02-15",
                  face_image = Nothing,
                  gender = Nothing,
                  hazardous_valid_till = Nothing,
                  hill_valid_till = Nothing,
                  id_number = Just "MH-123412341234",
                  issuing_rto_name = Just "MH, MOTIHARI",
                  last_transacted_at = Nothing,
                  name = Just "JOHN CARL DOE",
                  nt_validity_from = Just "2015-05-20",
                  nt_validity_to = Just "2035-02-14",
                  relatives_name = Nothing,
                  source = Just "SARATHI",
                  status = Just "id_found",
                  t_validity_from = Nothing,
                  t_validity_to = Nothing
                }
          }
    }
