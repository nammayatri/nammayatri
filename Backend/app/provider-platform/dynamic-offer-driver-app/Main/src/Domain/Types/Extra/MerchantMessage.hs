{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.MerchantMessage where

import Data.Default.Class
import Kernel.Prelude

-- Extra code goes here --
data MerchantMessageDefaultDataJSON = MerchantMessageDefaultDataJSON
  { var1 :: Maybe Text,
    var2 :: Maybe Text,
    var3 :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema, Eq)

instance Default MerchantMessageDefaultDataJSON where
  def =
    MerchantMessageDefaultDataJSON
      { var1 = Nothing,
        var2 = Nothing,
        var3 = Nothing
      }
