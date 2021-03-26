module Types.Error (module Types.Error) where

import Beckn.TypeClass.IsAPIError
import Beckn.Types.Error as Types.Error
import EulerHS.Prelude

data RatingError
  = InvalidRatingValue
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError RatingError where
  toAPIError InvalidRatingValue = APIError "INVALID_RATING_VALUE" "Invalid rating value."
  toStatusCode InvalidRatingValue = E400

data ServiceabilityError
  = ProductNotServiceable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError ServiceabilityError where
  toAPIError ProductNotServiceable = APIError "PRODUCT_NOT_SERVICEABLE" "Requested product is not serviceable for some reason."
  toStatusCode ProductNotServiceable = E400