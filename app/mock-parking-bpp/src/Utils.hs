module Utils where

import Beckn.Types.Amount
import qualified Beckn.Types.Core.Migration.DecimalValue as Core
import Core.Common.Price
import Data.String.Conversions
import Relude hiding (id, state)

buildPrice :: (Integral a) => a -> Price
buildPrice x =
  Price
    { currency = "INR",
      value = Core.convertAmountToDecimalValue $ Amount $ fromIntegral x
    }

buildPriceAmount :: Amount -> Price
buildPriceAmount x =
  Price
    { currency = "INR",
      value = Core.convertAmountToDecimalValue x
    }

validateUnique :: Text -> [a] -> Either Text a
validateUnique _ [x] = Right x
validateUnique entity [] = Left $ "empty " <> entity <> " list"
validateUnique entity _ = Left $ "expected one " <> entity
