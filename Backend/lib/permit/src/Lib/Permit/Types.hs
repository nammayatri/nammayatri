-- | Types shared by every permit type.
module Lib.Permit.Types where

import Data.Aeson (eitherDecode, encode)
import qualified Data.Aeson as A
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

-- | The kinds of permit. Written by hand because, while there is only one kind, the
-- derived JSON of a single-constructor type is an empty array instead of its name.
data PermitType = PICNIC
  deriving (Eq, Ord, Show, Read, Generic, ToParamSchema)

permitTypeJson :: A.Options
permitTypeJson = A.defaultOptions {A.tagSingleConstructors = True}

instance ToJSON PermitType where
  toJSON = A.genericToJSON permitTypeJson

instance FromJSON PermitType where
  parseJSON = A.genericParseJSON permitTypeJson

instance ToSchema PermitType where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions permitTypeJson)

$(mkBeamInstancesForEnumAndList ''PermitType)

$(mkHttpInstancesForEnum ''PermitType)

-- | A rule that stops an application. 'clause' is the SOP section, or "FORM" for
-- a field that is missing or malformed.
data RuleFailure = RuleFailure
  { code :: Text,
    clause :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data ScanResult = GREEN | AMBER | RED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data ScanReason = ScanReason
  { code :: Text,
    message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
