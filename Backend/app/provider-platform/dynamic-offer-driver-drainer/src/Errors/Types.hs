{-# LANGUAGE RecordWildCards #-}

module Errors.Types where

import Control.Exception (Exception (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toEncoding, toJSON),
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import EulerHS.Extra.Aeson (aesonOmitNothingFields)
import EulerHS.Extra.AltValidation (VErrorPayload (..))
import GHC.Generics (Generic)
import Prelude hiding (error, show)

-- from src/Engineering/Types/API.purs

data ErrorResponse = ErrorResponse
  { code :: Int,
    response :: ErrorPayload
  }
  deriving (Eq, Show, Generic)

instance Exception ErrorResponse

data ECErrorResponse = ECErrorResponse
  { code :: Int,
    response :: BSL.ByteString
  }
  deriving (Eq, Show, Generic)

instance Exception ECErrorResponse

-- ----------------------------------------------------------------------------

data ErrorPayload = ErrorPayload
  { error :: Bool,
    errorMessage :: Text,
    userMessage :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ErrorPayload where
  toJSON = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON ErrorPayload where
  parseJSON = genericParseJSON aesonOmitNothingFields

data ECErrorPayload = ECErrorPayload
  { status :: Text,
    error_code :: Maybe Text,
    error_message :: Maybe Text,
    status_id :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON ECErrorPayload where
  toJSON = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON ECErrorPayload where
  parseJSON = genericParseJSON aesonOmitNothingFields

toVErr :: ECErrorPayload -> VErrorPayload
toVErr ECErrorPayload {..} =
  VErrorPayload
    { status = status,
      status_id = status_id,
      error_code = error_code,
      error_message = error_message,
      error_field = Nothing
    }

toECErr :: Int -> ECErrorPayload -> ECErrorResponse
toECErr i payload =
  ECErrorResponse
    { code = i,
      response = encodePretty payload
    }

-- ----------------------------------------------------------------------------

data ECOrderStatusErrorPayload = ECOrderStatusErrorPayload
  { status :: Text,
    error_code :: Maybe Text,
    error_message :: Maybe Text,
    status_id :: Maybe Int,
    order_id :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ECOrderStatusErrorPayload where
  toJSON = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON ECOrderStatusErrorPayload where
  parseJSON = genericParseJSON aesonOmitNothingFields

-- ----------------------------------------------------------------------------

data ECTxnStatusErrorPayload = ECTxnStatusErrorPayload
  { status :: Text,
    error_code :: Maybe Text,
    error_message :: Maybe Text,
    status_id :: Maybe Int,
    txn_uuid :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ECTxnStatusErrorPayload where
  toJSON = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON ECTxnStatusErrorPayload where
  parseJSON = genericParseJSON aesonOmitNothingFields

-- ----------------------------------------------------------------------------

data TxnValidationErrorResp = TxnValidationErrorResp
  { error_code :: Text,
    error_message :: Text
  }
  deriving (Eq, Show, Generic)

instance Exception TxnValidationErrorResp

instance ToJSON TxnValidationErrorResp where
  toJSON = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON TxnValidationErrorResp where
  parseJSON = genericParseJSON aesonOmitNothingFields
