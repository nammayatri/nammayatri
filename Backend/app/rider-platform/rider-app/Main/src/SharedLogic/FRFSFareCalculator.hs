module SharedLogic.FRFSFareCalculator where

import Domain.Action.Beckn.FRFS.Common
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Kernel.Prelude
import Kernel.Types.Common

data CategoryPriceItem = CategoryPriceItem
  { quantity :: Int,
    unitPrice :: Price,
    totalPrice :: Price,
    categoryType :: FRFSQuoteCategoryType
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FRFSFareParameters = FRFSFareParameters
  { priceItems :: [CategoryPriceItem],
    totalPrice :: Price,
    totalQuantity :: Int,
    currency :: Currency
  }
  deriving (Generic, Show, ToJSON, FromJSON)

mkCategoryPriceItemFromQuoteCategories :: [FRFSQuoteCategory] -> [CategoryPriceItem]
mkCategoryPriceItemFromQuoteCategories quoteCategories = map mkPriceItem quoteCategories
  where
    mkPriceItem :: FRFSQuoteCategory -> CategoryPriceItem
    mkPriceItem category =
      let unitPrice = fromMaybe category.offeredPrice category.finalPrice
       in CategoryPriceItem
            { quantity = category.selectedQuantity,
              unitPrice,
              totalPrice = modifyPrice unitPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) * (toRational category.selectedQuantity),
              categoryType = category.category
            }

mkCategoryPriceItemFromDCategorySelect :: [DCategorySelect] -> [CategoryPriceItem]
mkCategoryPriceItemFromDCategorySelect quoteCategories = mapMaybe mkPriceItem quoteCategories
  where
    mkPriceItem :: DCategorySelect -> Maybe CategoryPriceItem
    mkPriceItem category = do
      let quantity = category.quantity
      let unitPrice = category.price
      return $
        CategoryPriceItem
          { quantity = quantity,
            unitPrice,
            totalPrice = modifyPrice unitPrice $ \p -> HighPrecMoney $ (p.getHighPrecMoney) * (toRational quantity),
            categoryType = category.category
          }

mkFareParameters :: [CategoryPriceItem] -> FRFSFareParameters
mkFareParameters priceItems =
  let totalPrice =
        Price
          { amount = sum ((.totalPrice.amount) <$> priceItems),
            amountInt = sum ((.totalPrice.amountInt) <$> priceItems),
            currency = fromMaybe INR (map (.unitPrice.currency) listToMaybe priceItems)
          }
      totalQuantity = sum ((.quantity) <$> priceItems)
   in FRFSFareParameters
        { priceItems = priceItems,
          totalPrice = totalPrice,
          totalQuantity = totalQuantity,
          currency = totalPrice.currency
        }
