{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Validation
  ( module Beckn.Utils.Validation,
    module Beckn.Types.Validation,
  )
where

import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Logging
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Error.Throwing
import qualified Data.Either.Validation as V
import EulerHS.Prelude hiding (pred)

runRequestValidation ::
  (MonadThrow m, Log m) =>
  Validate obj ->
  obj ->
  m ()
runRequestValidation validator obj =
  V.validationToEither (validator obj)
    & fromEitherM RequestValidationFailure

newtype RequestValidationFailure = RequestValidationFailure [ValidationDescription]
  deriving (Show, IsBaseError)

instance IsAPIError RequestValidationFailure where
  toErrorCode (RequestValidationFailure _failures) = "REQUEST_VALIDATION_FAILURE"
  toHttpCode (RequestValidationFailure _failures) = E400
  toPayload (RequestValidationFailure failures) = toJSON failures

instanceExceptionWithParent 'APIException ''RequestValidationFailure

validateField ::
  (Predicate a p, ShowablePredicate p) =>
  Text ->
  a ->
  p ->
  Validation
validateField fieldName fieldValue pred =
  unless (pFun pred fieldValue) . V.Failure $ [validationDescription]
  where
    validationDescription =
      ValidationDescription
        { fieldName = [fieldName],
          expectation = pShow pred fieldName
        }

validateObject ::
  Text ->
  a ->
  Validate a ->
  Validation
validateObject fieldName object validator = first addPrefixes $ validator object
  where
    addPrefixes = map (addPrefixToFieldName fieldName)

addPrefixToFieldName ::
  Text ->
  ValidationDescription ->
  ValidationDescription
addPrefixToFieldName prefix = #fieldName %~ (prefix :)
