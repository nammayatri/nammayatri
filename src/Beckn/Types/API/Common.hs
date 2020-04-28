module Beckn.Types.API.Common where

import Data.Swagger
import Data.Default
import EulerHS.Prelude
import Data.Aeson
import Servant
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.ByteString.Lazy as BSL
import Servant.Swagger
import Data.Swagger

data ErrorResponse =
  ErrorResponse
    { status :: Text
    , responseCode :: Text
    , responseMessage :: Text
    }
  deriving (Show, Generic, ToJSON, ToSchema)

data LoginMode
  = VERIFY
  | RESEND
  deriving (Generic, FromJSON, ToSchema)

data PassApplicationType
  = SELF
  | SPONSOROR
  | BULKSPONSOROR
  deriving (Generic, FromJSON, ToSchema)

instance Default PassApplicationType where
  def = SELF

data TravellerIDType
  = MOBILE
  | AADHAR
  deriving (Generic, FromJSON, ToSchema)

instance Default TravellerIDType where
  def = MOBILE

data PassAction
  = REVOKE
  | EXPIRE
  deriving (Generic, FromJSON, ToSchema)

data PassIDType
  = MOBILENUMBER
  | CUSTOMERID
  | PASSAPPLICATIONID
  deriving (Generic, FromJSON, ToSchema)

instance ToParamSchema PassIDType
instance FromHttpApiData PassIDType where
  parseUrlPiece  = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = bimap T.pack id . eitherDecode . BSL.fromStrict
