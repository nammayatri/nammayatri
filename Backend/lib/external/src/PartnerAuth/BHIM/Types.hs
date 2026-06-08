module PartnerAuth.BHIM.Types where

import Kernel.Prelude

-- | S2S AES-encrypted envelope (request body).
newtype EncEnvelope = EncEnvelope {encRequest :: Text}
  deriving (Show, Generic, ToJSON, FromJSON)

-- | S2S AES-encrypted envelope (response body).
newtype EncEnvelopeRes = EncEnvelopeRes {encResponseBody :: Text}
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Inner (decrypted) request for both verify-token and user-details.
newtype TokenReq = TokenReq {token :: Text}
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Inner (decrypted) response of verify-token.
newtype VerifyTokenInnerRes = VerifyTokenInnerRes {isValid :: Bool}
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Inner (decrypted) response of user-details.
data UserDetailsInnerRes = UserDetailsInnerRes
  { name :: Text,
    mobileNumber :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Inner (decrypted) error body. BHIM returns errors as HTTP 200 with an
-- encrypted body that decrypts to this shape (errorCode in "400"|"401"|"500").
data BhimErrorBody = BhimErrorBody
  { userMessage :: Maybe Text,
    message :: Maybe Text,
    timeout :: Maybe Int,
    errorCode :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
