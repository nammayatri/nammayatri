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

newtype MerchantError
  = MerchantNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantError

instance IsBaseError MerchantError where
  toMessage (MerchantNotFound merchantId) = Just $ "Merchant with merchantId \"" <> show merchantId <> "\" not found."

instance IsHTTPError MerchantError where
  toErrorCode _ = "MERCHANT_NOT_FOUND"
  toHttpCode _ = E400

instance IsAPIError MerchantError

--
newtype SelectedQuoteError
  = SelectedQuoteNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SelectedQuoteError

instance IsBaseError SelectedQuoteError where
  toMessage (SelectedQuoteNotFound qId) = Just $ "Selected quote with id \"" <> show qId <> "\" not found."

instance IsHTTPError SelectedQuoteError where
  toErrorCode _ = "SELECTED_QUOTE_NOT_FOUND"
  toHttpCode _ = E400

instance IsAPIError SelectedQuoteError
