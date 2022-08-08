module Types.API.DriverOnboarding.Status where

import Beckn.Prelude

data ResponseStatus = VERIFICATION_PENDING | VERIFIED | VERIFICATION_FAILED | WAITING_INPUT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    rcVerificationStatus :: ResponseStatus,
    operatingCity :: Text
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)
