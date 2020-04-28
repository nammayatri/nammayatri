module Beckn.Types.API.Common where

import Data.Default
import EulerHS.Prelude
import Data.Aeson
import Servant
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.ByteString.Lazy as BSL

data ErrorResponse =
  ErrorResponse
    { status :: Text
    , responseCode :: Text
    , responseMessage :: Text
    }
  deriving (Show, Generic, ToJSON)

data Medium
  = SMS
  | EMAIL
  deriving (Generic, FromJSON)

data LoginType
  = OTP
  | PASSWORD
  deriving (Generic, FromJSON)

data LoginMode
  = VERIFY
  | RESEND
  deriving (Generic, FromJSON)

data PassApplicationType
  = SELF
  | SPONSOROR
  | BULKSPONSOROR
  deriving (Generic, FromJSON)

instance Default PassApplicationType where
  def = SELF

data TravellerIDType
  = MOBILE
  | AADHAR
  deriving (Generic, FromJSON)

instance Default TravellerIDType where
  def = MOBILE

data PassAction
  = REVOKE
  | EXPIRE
  deriving (Generic, FromJSON)

data PassIDType
  = MOBILENUMBER
  | CUSTOMERID
  | PASSAPPLICATIONID
  deriving (Generic, FromJSON)

instance FromHttpApiData PassIDType where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict
