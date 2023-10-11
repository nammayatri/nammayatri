{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Plan where

import Data.Aeson
import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

newtype RideCountBasedFeePolicyConfig = RideCountBasedFeePolicyConfig [RideCountBasedFeePolicy] deriving (Generic, ToJSON, FromJSON, Show)

data Plan = Plan
  { id :: Id Plan,
    paymentMode :: PaymentMode,
    merchantId :: Id DMerchant.Merchant,
    name :: Text,
    description :: Text,
    maxAmount :: HighPrecMoney,
    registrationAmount :: HighPrecMoney,
    isOfferApplicable :: Bool,
    maxCreditLimit :: HighPrecMoney,
    planBaseAmount :: PlanBaseAmount,
    freeRideCount :: Int,
    frequency :: Frequency,
    planType :: PlanType,
    cgstPercentage :: HighPrecMoney,
    sgstPercentage :: HighPrecMoney
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

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

data PaymentMode = MANUAL | AUTOPAY deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data Frequency = DAILY | WEEKLY | MONTHLY
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)

data PlanType = DEFAULT | SUBSCRIPTION
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)

data PlanBaseAmount
  = PERRIDE_BASE HighPrecMoney
  | DAILY_BASE HighPrecMoney
  | WEEKLY_BASE HighPrecMoney
  | MONTHLY_BASE HighPrecMoney
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlanBaseAmount

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

instance Show PlanBaseAmount where
  show (PERRIDE_BASE amount) = "PERRIDE_" <> T.unpack (show amount)
  show (DAILY_BASE amount) = "DAILY_" <> T.unpack (show amount)
  show (WEEKLY_BASE amount) = "WEEKLY_" <> T.unpack (show amount)
  show (MONTHLY_BASE amount) = "MONTHLY_" <> T.unpack (show amount)

$(mkBeamInstancesForEnum ''PaymentMode)

$(mkBeamInstancesForEnum ''Frequency)

$(mkBeamInstancesForEnum ''PlanType)

$(mkBeamInstancesForEnum ''PlanBaseAmount)

$(mkHttpInstancesForEnum ''Frequency)

$(mkHttpInstancesForEnum ''PaymentMode)

$(mkHttpInstancesForEnum ''PlanType)
