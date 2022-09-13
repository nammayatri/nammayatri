module Idfy.Types.ExtractImage where

import Beckn.Utils.JSON
import Data.Aeson hiding (Error)
import Data.OpenApi
import EulerHS.Prelude hiding (state)
import Idfy.Types.VerificationResult

data ExtractImageReq = ExtractImageReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyDocData
  }
  deriving (Show, Generic)

instance ToSchema ExtractImageReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ExtractImageReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ExtractImageReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data VerifyDocData = VerifyDLwithSourceData
  { document1 :: Text,
    document2 :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ExtractedRCDetails = ExtractedRCDetails
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
    status :: Text
  }
  deriving (Show, Generic)

instance ToSchema ExtractedRCDetails where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ExtractedRCDetails where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ExtractedRCDetails where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ExtractedDLDetails = ExtractedDLDetails
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
    issue_dates :: ValidateIssueDate,
    _type :: [ClassOfVehicle],
    validity :: Validity,
    status :: Text
  }
  deriving (Show, Generic)

instance ToSchema ExtractedDLDetails where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ExtractedDLDetails where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ExtractedDLDetails where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Validity = Validity
  { nt :: Maybe Text,
    t :: Maybe Text
  }
  deriving (Show, Generic)

data ValidateIssueDate = ValidateIssueDate
  { lmv :: Maybe Text,
    mcwg :: Maybe Text,
    trans :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema Validity where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON Validity where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON Validity where
  toJSON = genericToJSON constructorsToUpperOptions

instance ToSchema ValidateIssueDate where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions constructorsToUpperOptions

instance FromJSON ValidateIssueDate where
  parseJSON = genericParseJSON constructorsToUpperOptions

instance ToJSON ValidateIssueDate where
  toJSON = genericToJSON constructorsToUpperOptions
