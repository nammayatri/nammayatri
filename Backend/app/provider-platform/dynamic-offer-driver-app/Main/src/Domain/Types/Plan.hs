{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Plan where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money, fromFieldEnum)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Servant.API
import qualified Text.Show

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
    planType :: PlanType
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

instance FromBackendRow Postgres PaymentMode

instance FromField PaymentMode where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PaymentMode where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PaymentMode

data Frequency = DAILY | WEEKLY | MONTHLY
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)

instance FromField Frequency where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Frequency where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Frequency

instance FromBackendRow Postgres Frequency

instance IsString Frequency where
  fromString = show

data PlanType = DEFAULT | SUBSCRIPTION
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToParamSchema)

instance FromField PlanType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PlanType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PlanType

instance FromBackendRow Postgres PlanType

instance IsString PlanType where
  fromString = show

data PlanBaseAmount
  = PERRIDE_BASE HighPrecMoney
  | DAILY_BASE HighPrecMoney
  | WEEKLY_BASE HighPrecMoney
  | MONTHLY_BASE HighPrecMoney
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlanBaseAmount

instance FromField PlanBaseAmount where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PlanBaseAmount where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PlanBaseAmount

instance FromBackendRow Postgres PlanBaseAmount

instance IsString PlanBaseAmount where
  fromString = show

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

instance FromHttpApiData Frequency where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData Frequency where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData PaymentMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData PaymentMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData PlanType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData PlanType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
