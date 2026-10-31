module Domain.Types.PassOverrideConfig where

import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Utils.JSON (constructorsWithSnakeCase)

newtype OverrideBenefitConfig = OverrideBenefitConfig
  { overrideBenefits :: [OverrideBenefitEntry]
  }
  deriving (Generic, Show, Eq, ToSchema)

data OverrideBenefitEntry
  = TicketBenefit OverrideBenefit
  | PassTicketBenefit PassTicketBenefitConfig
  deriving (Generic, Show, Eq, ToSchema)

data PassTicketBenefitConfig = PassTicketBenefitConfig
  { pricing :: PassPricingConfig,
    benefit :: OverrideBenefit
  }
  deriving (Generic, Show, Eq, ToSchema)

data PassPricingConfig = PassPricingConfig
  { percentageSaving :: Maybe PercentageSaving,
    fixedSaving :: Maybe FixedSaving,
    primaryServiceTier :: Spec.ServiceTierType,
    maximumPurchaseableTripCount :: Int,
    variableTripCount :: Maybe Bool,
    tripCountRecommendations :: Maybe [Int],
    unlimitedPricedTripCount :: Maybe Int,
    allowRoundTripPurchase :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToSchema)

data OverrideBenefit = OverrideBenefit
  { percentageSaving :: Maybe PercentageSaving,
    fixedSaving :: Maybe FixedSaving,
    unlimitedTripCount :: Maybe Bool,
    maximumTripCount :: Maybe Int,
    maxTicketQuantityPerOverride :: Maybe Int,
    enforceStopPair :: Maybe Bool,
    allowRoundTrip :: Maybe Bool,
    allowCheaperStopPairs :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToSchema)

data PercentageSaving = PercentageSaving
  { applicableValue :: HighPrecMoney,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToSchema)

data FixedSaving = FixedSaving
  { applicableValue :: HighPrecMoney,
    currencyType :: Maybe Currency,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, Eq, ToSchema)

instance FromJSON OverrideBenefitConfig where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON OverrideBenefitConfig where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON OverrideBenefitEntry where
  parseJSON value@(Object obj)
    | KM.member "tag" obj = genericParseJSON constructorsWithSnakeCase value
  parseJSON value = TicketBenefit <$> parseJSON value

instance ToJSON OverrideBenefitEntry where
  toJSON (TicketBenefit benefit) = toJSON benefit
  toJSON entry = genericToJSON constructorsWithSnakeCase entry

instance FromJSON PassTicketBenefitConfig where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PassTicketBenefitConfig where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PassPricingConfig where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PassPricingConfig where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON OverrideBenefit where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON OverrideBenefit where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON PercentageSaving where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PercentageSaving where
  toJSON = genericToJSON constructorsWithSnakeCase

instance FromJSON FixedSaving where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON FixedSaving where
  toJSON = genericToJSON constructorsWithSnakeCase

entryBenefit :: OverrideBenefitEntry -> OverrideBenefit
entryBenefit (TicketBenefit benefit) = benefit
entryBenefit (PassTicketBenefit config) = config.benefit

entryPricing :: OverrideBenefitEntry -> Maybe PassPricingConfig
entryPricing (TicketBenefit _) = Nothing
entryPricing (PassTicketBenefit config) = Just config.pricing

firstEntry :: OverrideBenefitConfig -> Maybe OverrideBenefitEntry
firstEntry config = listToMaybe config.overrideBenefits
