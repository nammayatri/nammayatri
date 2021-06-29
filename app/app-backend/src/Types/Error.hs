{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Error.BaseError.APIError.DomainError
import EulerHS.Prelude

data RatingError
  = InvalidRatingValue
  deriving (Eq, Show)

instanceExceptionWithParent 'DomainException ''RatingError

instance IsBaseError RatingError

instance IsAPIError RatingError where
  toErrorCode InvalidRatingValue = "INVALID_RATING_VALUE"
  toHttpCode InvalidRatingValue = E400

instance IsDomainError RatingError

newtype ServiceabilityError
  = ProductNotServiceable Text
  deriving (Eq, Show)

instanceExceptionWithParent 'DomainException ''ServiceabilityError

instance IsBaseError ServiceabilityError where
  toMessage (ProductNotServiceable reason) = Just $ "Requested product is not serviceable " <> reason <> "."

instance IsAPIError ServiceabilityError where
  toErrorCode (ProductNotServiceable _) = "PRODUCT_NOT_SERVICEABLE"
  toHttpCode (ProductNotServiceable _) = E400

instance IsDomainError ServiceabilityError
