{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Idfy.Types.ValidateImage where

import Beckn.Utils.JSON
import Data.Aeson hiding (Error)
import Data.OpenApi
import EulerHS.Prelude hiding (state)

data ValidateImageReq = ValidateRCReq
  { task_id :: Text,
    group_id :: Text,
    _data :: ValidateDoc
  }
  deriving (Show, Generic)

instance ToSchema ValidateImageReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ValidateImageReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ValidateImageReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ValidateDoc = ValidateDoc
  { document1 :: Text,
    doc_type :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data ValidateImageRes = ValidateImageRes
  { action :: Maybe Text,
    completed_at :: Maybe Text,
    created_at :: Maybe Text,
    group_id :: Maybe Text,
    request_id :: Maybe Text,
    result :: ResultBody,
    status :: Text,
    task_id :: Maybe Text,
    _type :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema ValidateImageRes where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON ValidateImageRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ValidateImageRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ResultBody = ResultBody
  { detected_doc_type :: Maybe Text,
    is_readable :: Maybe Bool,
    readability :: ReadabilityBody
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ReadabilityBody = ReadabilityBody {confidence :: Maybe Int} deriving (Show, Generic)

deriving newtype instance ToJSON ReadabilityBody

deriving newtype instance FromJSON ReadabilityBody

deriving newtype instance ToSchema ReadabilityBody
