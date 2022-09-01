{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Idfy.Types.VerifyReq where

import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.OpenApi
  ( ToSchema (..),
    fromAesonOptions,
    genericDeclareNamedSchema,
  )
import EulerHS.Prelude

data VerifyReq a = VerifyReq
  { task_id :: Text,
    group_id :: Text,
    _data :: a
  }
  deriving (Show, Generic)

instance (ToSchema a) => ToSchema (VerifyReq a) where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance (FromJSON a) => FromJSON (VerifyReq a) where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance (ToJSON a) => ToJSON (VerifyReq a) where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data VerifyDLData = VerifyDLData
  { id_number :: Text,
    date_of_birth :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype VerifyRCData = VerifyRCData {rc_number :: Text}
  deriving (Show, Generic)

deriving newtype instance ToJSON VerifyRCData

deriving newtype instance FromJSON VerifyRCData

deriving newtype instance ToSchema VerifyRCData