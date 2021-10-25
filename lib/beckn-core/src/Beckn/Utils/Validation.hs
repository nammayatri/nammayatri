{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Validation
  ( module Beckn.Utils.Validation,
    module Beckn.Types.Validation,
  )
where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Logging
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Error.Throwing
import qualified Data.Either.Validation as V
import Data.Generics.Labels ()
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
  deriving (Show, IsBaseError, IsBecknAPIError)

instance IsHTTPError RequestValidationFailure where
  toErrorCode (RequestValidationFailure _failures) = "REQUEST_VALIDATION_FAILURE"
  toHttpCode (RequestValidationFailure _failures) = E400

instance IsAPIError RequestValidationFailure where
  toPayload (RequestValidationFailure failures) = toJSON failures

instanceExceptionWithParent 'HTTPException ''RequestValidationFailure

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
validateObject fieldName object validator = addPrefixes fieldName $ validator object

validateList ::
  Container a =>
  Text ->
  a ->
  Validate (Element a) ->
  Validation
validateList fieldName list validator =
  traverse_ f (zip (map (\i -> fieldName <> "[" <> show i <> "]") [0 :: Int ..]) $ toList list)
  where
    f (pref, val) = addPrefixes pref $ validator val

addPrefixes :: Text -> Validation -> Validation
addPrefixes fieldName = first $ map (addPrefixToFieldName fieldName)

addPrefixToFieldName ::
  Text ->
  ValidationDescription ->
  ValidationDescription
addPrefixToFieldName prefix = #fieldName %~ (prefix :)
