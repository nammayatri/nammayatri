{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.BaseError.HTTPError
import EulerHS.Prelude

data RatingError
  = InvalidRatingValue
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RatingError

instance IsBaseError RatingError

instance IsHTTPError RatingError where
  toErrorCode InvalidRatingValue = "INVALID_RATING_VALUE"
  toHttpCode InvalidRatingValue = E400

instance IsAPIError RatingError

newtype ServiceabilityError
  = ProductNotServiceable Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ServiceabilityError

instance IsBaseError ServiceabilityError where
  toMessage (ProductNotServiceable reason) = Just $ "Requested product is not serviceable " <> reason <> "."

instance IsHTTPError ServiceabilityError where
  toErrorCode (ProductNotServiceable _) = "PRODUCT_NOT_SERVICEABLE"
  toHttpCode (ProductNotServiceable _) = E400

instance IsAPIError ServiceabilityError
