{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Idfy.Types.Error where

import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))
import Prelude

data IdfyError
  = IdfyNotConfigured
  | IdfyBadRequest
  | IdfyInvalidCredentials
  | IdfyMissingCredentials
  | IdfyNotFound
  | IdfySizeLimitExceed
  | IdfyUnprocessableEntity
  | IdfyRateLimitExceed
  | IdfyInternalServer
  | IdfyBadGateway
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IdfyError

instance FromResponse IdfyError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just IdfyBadRequest
    401 -> Just IdfyMissingCredentials
    403 -> Just IdfyInvalidCredentials
    404 -> Just IdfyNotFound
    413 -> Just IdfySizeLimitExceed
    422 -> Just IdfyUnprocessableEntity
    429 -> Just IdfyRateLimitExceed
    500 -> Just IdfyInternalServer
    502 -> Just IdfyBadGateway
    _ -> Just IdfyInternalServer

instance IsBaseError IdfyError where
  toMessage = \case
    IdfyNotConfigured -> Just "Idfy env variables aren't properly set."
    IdfyBadRequest -> Just "Bad request. Please check for the input."
    IdfyInvalidCredentials -> Just "Something went wrong on our end. Please try again."
    IdfyMissingCredentials -> Just "Something went wrong on our end. Please try again."
    IdfyNotFound -> Just "Something went wrong on our end. Please try again."
    IdfySizeLimitExceed -> Just "Image size is more than 2MB. Please check for the size."
    IdfyUnprocessableEntity -> Just "Unprocessable image. Please check for the image."
    IdfyRateLimitExceed -> Just "Something went wrong on our end. Please try again."
    IdfyInternalServer -> Just "Something went wrong on our end. Please try again."
    IdfyBadGateway -> Just "Something went wrong on our end. Please try again."

instance IsHTTPError IdfyError where
  toErrorCode = \case
    IdfyNotConfigured -> "IDFY_NOT_CONFIGURED"
    IdfyBadRequest -> "BAD_REQUEST"
    IdfyInvalidCredentials -> "INTERNAL_SERVER_ERROR"
    IdfyMissingCredentials -> "INTERNAL_SERVER_ERROR"
    IdfyNotFound -> "INTERNAL_SERVER_ERROR"
    IdfySizeLimitExceed -> "SIZE_LIMIT_EXCEED"
    IdfyUnprocessableEntity -> "UNPROCESSABLE_ENTITY"
    IdfyRateLimitExceed -> "INTERNAL_SERVER_ERROR"
    IdfyInternalServer -> "INTERNAL_SERVER_ERROR"
    IdfyBadGateway -> "INTERNAL_SERVER_ERROR"

  toHttpCode = \case
    IdfyNotConfigured -> E500
    IdfyBadRequest -> E400
    IdfyInvalidCredentials -> E500
    IdfyMissingCredentials -> E500
    IdfyNotFound -> E500
    IdfySizeLimitExceed -> E400
    IdfyUnprocessableEntity -> E400
    IdfyRateLimitExceed -> E500
    IdfyInternalServer -> E500
    IdfyBadGateway -> E500

instance IsAPIError IdfyError
