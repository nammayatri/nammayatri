module Storage.Queries.Transformers.MerchantMessage where

import qualified Data.Aeson
import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import qualified Domain.Types.MerchantMessage
import Kernel.Prelude

valueToJsonData :: (Kernel.Prelude.Maybe Data.Aeson.Value -> Domain.Types.MerchantMessage.MerchantMessageDefaultDataJSON)
valueToJsonData jsonData = fromMaybe def (valueToJsonData' =<< jsonData)
  where
    valueToJsonData' :: A.Value -> Maybe Domain.Types.MerchantMessage.MerchantMessageDefaultDataJSON
    valueToJsonData' value = case A.fromJSON value of
      A.Error _ -> Nothing
      A.Success a -> Just a
