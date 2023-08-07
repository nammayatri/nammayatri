{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.PlanDetails where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Money)
import Kernel.Types.Id
import Servant.API

newtype CriteriaConfig = CriteriaConfig RideCountBasedFeePolicyConfig deriving (Generic, ToJSON, FromJSON, Show)

newtype RideCountBasedFeePolicyConfig = RideCountBasedFeePolicyConfig
  { rideCountBasedFeePolicy :: [RideCountBasedFeePolicy]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PlanDetails = PlanDetails
  { id :: Id PlanDetails,
    paymentMode :: PaymentMode,
    merchantId :: Id DMerchant.Merchant,
    name :: Text,
    description :: Text,
    maxAmount :: Money,
    maxCreditLimit :: Money,
    driverPaymentCycleDuration :: Int,
    driverPaymentCycleStart :: Int,
    criteriaConfig :: CriteriaConfig
  }
  deriving (Generic, Show)

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

data PaymentMode = MANUAL | AUTOPAY deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)

data Frequency = DAILY | WEEKLY | MONTHLY deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)

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
