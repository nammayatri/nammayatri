module Core.ACL.Common where

import Beckn.Prelude
import Types.Error
import Utils.Common

validatePrices :: (MonadThrow m, Log m, Num a, Ord a) => a -> a -> m ()
validatePrices price priceWithDiscount = do
  when (price < 0) $ throwError $ InvalidRequest "price is less than zero"
  when (priceWithDiscount < 0) $ throwError $ InvalidRequest "discounted price is less than zero"
  when (priceWithDiscount > price) $ throwError $ InvalidRequest "price is lesser than discounted price"
