{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.MerchantMessage where

import qualified Data.Aeson
import qualified Data.Aeson as A
import Data.Default.Class (Default (..))
import qualified Domain.Types.MerchantMessage
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)

valueToJsonData :: (Kernel.Prelude.Maybe Data.Aeson.Value -> Domain.Types.MerchantMessage.MerchantMessageDefaultDataJSON)
valueToJsonData jsonData = fromMaybe def (valueToJsonData' =<< jsonData)
  where
    valueToJsonData' :: A.Value -> Maybe Domain.Types.MerchantMessage.MerchantMessageDefaultDataJSON
    valueToJsonData' value = case A.fromJSON value of
      A.Error _ -> Nothing
      A.Success a -> Just a
