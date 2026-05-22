module Email.OTP.Types where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data SendEmailOTPReq = SendEmailOTPReq
  { email :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype SendEmailOTPRes = SendEmailOTPRes
  { otp :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)
