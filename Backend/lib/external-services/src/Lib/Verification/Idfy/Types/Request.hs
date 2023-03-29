{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Lib.Verification.Idfy.Types.Request where

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
