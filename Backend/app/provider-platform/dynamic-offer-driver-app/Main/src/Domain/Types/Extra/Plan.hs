{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Extra.Plan where

import Data.Aeson
import qualified Data.List as List
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.GenericPretty
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

instance Show PlanBaseAmount where
  show (PERRIDE_BASE amount) = "PERRIDE_" <> T.unpack (show amount)
  show (DAILY_BASE amount) = "DAILY_" <> T.unpack (show amount)
  show (WEEKLY_BASE amount) = "WEEKLY_" <> T.unpack (show amount)
  show (MONTHLY_BASE amount) = "MONTHLY_" <> T.unpack (show amount)

data PlanBaseAmount
  = PERRIDE_BASE HighPrecMoney
  | DAILY_BASE HighPrecMoney
  | WEEKLY_BASE HighPrecMoney
  | MONTHLY_BASE HighPrecMoney
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlanBaseAmount

$(mkBeamInstancesForEnum ''PlanBaseAmount)

instance Read PlanBaseAmount where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (PERRIDE_BASE (read r1 :: HighPrecMoney), "")
            | r1 <- stripPrefix "PERRIDE_" r
          ]
            ++ [ (DAILY_BASE (read r1 :: HighPrecMoney), "")
                 | r1 <- stripPrefix "DAILY_" r
               ]
            ++ [ (WEEKLY_BASE (read r1 :: HighPrecMoney), "")
                 | r1 <- stripPrefix "WEEKLY_" r
               ]
            ++ [ (MONTHLY_BASE (read r1 :: HighPrecMoney), "")
                 | r1 <- stripPrefix "MONTHLY_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

newtype RideCountBasedFeePolicyConfig = RideCountBasedFeePolicyConfig [RideCountBasedFeePolicy] deriving (Generic, ToJSON, FromJSON, Show)

data RideCountBasedFeePolicy = RideCountBasedFeePolicy
  { baseRideCount :: Int,
    platformFee :: Money,
    platformFeeCgst :: HighPrecMoney,
    platformFeeSgst :: HighPrecMoney,
    perRideFee :: Money,
    perRideCgst :: HighPrecMoney,
    perRideSgst :: HighPrecMoney
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)
