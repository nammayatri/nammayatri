module Lib.JourneyModule.Types where

data JourneyInitData leg = JourneyInitData
  { legs: [leg]
  , parentSearchId :: Id SearchRequest
  , merchantId :: Id Merchant
  , merchantOperatingCityId :: Id MerchantOperatingCity
  }