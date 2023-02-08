{-# LANGUAGE DerivingStrategies #-}

module Idfy.Types.Request where

import Data.OpenApi
  ( ToSchema (..),
    fromAesonOptions,
    genericDeclareNamedSchema,
  )
import EulerHS.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)

type ImageValidateRequest = IdfyRequest ValidateRequest

type ImageExtractRequest = IdfyRequest ExtractRequest

type DLVerificationRequest = IdfyRequest DLVerificationData

type RCVerificationRequest = IdfyRequest RCVerificationData

data IdfyRequest a = IdfyRequest
  { task_id :: Text,
    group_id :: Text,
    _data :: a
  }
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (IdfyRequest a) where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance (FromJSON a) => FromJSON (IdfyRequest a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (IdfyRequest a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

-- validate image request
data ValidateRequest = ValidateRequest
  { document1 :: Text,
    doc_type :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- extract image request
data ExtractRequest = ExtractRequest
  { document1 :: Text,
    document2 :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- DL verification request
data DLVerificationData = DLVerificationData
  { id_number :: Text,
    date_of_birth :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- RC verification request
data RCVerificationData = RCVerificationData
  {rc_number :: Text, _a :: Maybe Text}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
