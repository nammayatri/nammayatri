module Storage.Queries.Transformers.MerchantMessage where

import Data.Aeson
import Domain.Types.MerchantMessage
import Kernel.Prelude

valueToJsonData :: Value -> Maybe MerchantMessageDefaultDataJSON
valueToJsonData value = case fromJSON value of
  Error _ -> Nothing
  Success a -> Just a
