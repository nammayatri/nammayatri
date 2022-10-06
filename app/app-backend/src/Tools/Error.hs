{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Tools.Error (module Tools.Error) where

import Beckn.Types.Error as Tools.Error
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

data MerchantError
  = MerchantNotFound Text
  | MerchantDoesNotExist Text
  | MerchantWithExoPhoneNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantError

instance IsBaseError MerchantError where
  toMessage (MerchantNotFound merchantId) = Just $ "Merchant with merchantId \"" <> show merchantId <> "\" not found."
  toMessage (MerchantDoesNotExist merchantId) = Just $ "No merchant matches passed data " <> show merchantId <> "."
  toMessage (MerchantWithExoPhoneNotFound exoPhone) = Just $ "Merchant with ExoPhone \"" <> show exoPhone <> "\" not found."

instance IsHTTPError MerchantError where
  toErrorCode _ = "MERCHANT_NOT_FOUND"
  toHttpCode _ = E400

instance IsAPIError MerchantError

--
newtype EstimateError = EstimateDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''EstimateError

instance IsBaseError EstimateError where
  toMessage (EstimateDoesNotExist estimateId) = Just $ "No estimate matches passed data \"" <> show estimateId <> "\" not exist. "

instance IsHTTPError EstimateError where
  toErrorCode _ = "ESTIMATE_DOES_NOT_EXIST"
  toHttpCode _ = E400

instance IsAPIError EstimateError
