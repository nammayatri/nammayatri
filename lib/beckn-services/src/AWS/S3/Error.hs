{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AWS.S3.Error where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))
import Prelude

data S3Error
  = S3NotConfigured
  | S3BadRequest
  | S3Unauthorized
  | S3PaymentRequired
  | S3AccessDenied
  | S3NotFound
  | S3Conflict
  | S3TooManyRequests
  | S3ServerError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''S3Error

instance FromResponse S3Error where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just S3BadRequest
    401 -> Just S3Unauthorized
    402 -> Just S3PaymentRequired
    403 -> Just S3AccessDenied
    404 -> Just S3NotFound
    409 -> Just S3Conflict
    429 -> Just S3TooManyRequests
    _ -> Just S3ServerError

instance IsBaseError S3Error where
  toMessage = \case
    S3NotConfigured -> Just "S3 env variables aren't properly set."
    S3BadRequest -> Just "Something in your header or request body was malformed."
    S3Unauthorized -> Just "Necessary credentials were either missing or invalid."
    S3PaymentRequired -> Just "The action is not available on your plan, or you have exceeded usage limits for your current plan."
    S3AccessDenied -> Just "Your credentials are valid, but you don’t have access to the requested resource."
    S3NotFound -> Just "The object you’re requesting doesn’t exist."
    S3Conflict -> Just "You might be trying to update the same resource concurrently."
    S3TooManyRequests -> Just "You are calling our APIs more frequently than we allow."
    S3ServerError -> Just "Something went wrong on our end. Please try again."

instance IsHTTPError S3Error where
  toErrorCode = \case
    S3NotConfigured -> "S3_NOT_CONFIGURED"
    S3BadRequest -> "S3_BAD_REQUEST"
    S3Unauthorized -> "S3_UNAUTHORIZED"
    S3PaymentRequired -> "S3_PAYMENT_REQUIRED"
    S3AccessDenied -> "S3_ACCESS_DENIED"
    S3NotFound -> "S3_NOT_FOUND"
    S3Conflict -> "S3_CONFLICT"
    S3TooManyRequests -> "S3_TOO_MANY_REQUESTS"
    S3ServerError -> "S3_SERVER_ERROR"

instance IsAPIError S3Error
