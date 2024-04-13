{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.MerchantMessage where

import Data.Default.Class
import Domain.Types.Common (UsageSafety (..))
import Kernel.Prelude

-- Extra code goes here --

data MerchantMessageDefaultDataJSON = MerchantMessageDefaultDataJSON
  { var1 :: Maybe Text,
    var2 :: Maybe Text,
    var3 :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

instance Default MerchantMessageDefaultDataJSON where
  def =
    MerchantMessageDefaultDataJSON
      { var1 = Nothing,
        var2 = Nothing,
        var3 = Nothing
      }
