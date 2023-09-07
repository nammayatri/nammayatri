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

module Domain.Types.Estimate where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Servant.API
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Estimate = Estimate
  { id :: Id Estimate,
    requestId :: Id DSearchRequest.SearchRequest,
    merchantId :: Maybe (Id DM.Merchant),
    bppEstimateId :: Id BPPEstimate,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    totalFareRange :: FareRange,
    estimatedDuration :: Maybe Seconds,
    estimatedDistance :: Maybe HighPrecMeters,
    device :: Maybe Text,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    itemId :: Text,
    tripTerms :: Maybe DTripTerms.TripTerms,
    estimateBreakupList :: [EstimateBreakup],
    nightShiftInfo :: Maybe NightShiftInfo,
    status :: EstimateStatus,
    waitingCharges :: WaitingCharges,
    driversLocation :: [LatLong],
    specialLocationTag :: Maybe Text,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

data BPPEstimate

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    oldNightShiftCharge :: Centesimal, -- TODO: this field works wrong, value in it not always make sense, it have to be removed later
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup
  { id :: Id EstimateBreakup,
    estimateId :: Id Estimate,
    title :: Text,
    price :: EstimateBreakupPrice
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow, ToSchema)

data EstimateBreakupPrice = EstimateBreakupPrice
  { currency :: Text,
    value :: Money
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow, ToSchema)

data FareRange = FareRange
  { minFare :: Money,
    maxFare :: Money
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON, ToSchema)

newtype WaitingCharges = WaitingCharges
  { waitingChargePerMin :: Maybe Money
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateAPIEntity = EstimateAPIEntity
  { id :: Id Estimate,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    discount :: Maybe Money,
    totalFareRange :: FareRange,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    tripTerms :: [Text],
    estimateFareBreakup :: [EstimateBreakupAPIEntity],
    nightShiftRate :: Maybe NightShiftRateAPIEntity, -- TODO: doesn't make sense, to be removed
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: WaitingCharges,
    driversLatLong :: [LatLong],
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data NightShiftRateAPIEntity = NightShiftRateAPIEntity
  { nightShiftMultiplier :: Centesimal, -- TODO: this field works wrong, value in it not always make sense, it have to be removed later
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakupAPIEntity = EstimateBreakupAPIEntity
  { title :: Text,
    price :: Money
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkEstimateAPIEntity :: Estimate -> EstimateAPIEntity
mkEstimateAPIEntity Estimate {..} = do
  EstimateAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
      estimateFareBreakup = mkEstimateBreakupAPIEntity <$> estimateBreakupList,
      driversLatLong = driversLocation,
      nightShiftRate = mkNightShiftRateAPIEntity <$> nightShiftInfo,
      ..
    }

mkNightShiftRateAPIEntity :: NightShiftInfo -> NightShiftRateAPIEntity
mkNightShiftRateAPIEntity NightShiftInfo {..} = do
  NightShiftRateAPIEntity
    { nightShiftMultiplier = oldNightShiftCharge,
      ..
    }

mkEstimateBreakupAPIEntity :: EstimateBreakup -> EstimateBreakupAPIEntity
mkEstimateBreakupAPIEntity EstimateBreakup {..} = do
  EstimateBreakupAPIEntity
    { title = title,
      price = price.value
    }

data EstimateStatus = NEW | DRIVER_QUOTE_REQUESTED | CANCELLED | GOT_DRIVER_QUOTE | DRIVER_QUOTE_CANCELLED | COMPLETED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''EstimateStatus)

instance FromHttpApiData EstimateStatus where
  parseUrlPiece = parseHeader.DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData EstimateStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

isCancelled ::
  EstimateStatus ->
  Bool
isCancelled status = status == CANCELLED || status == DRIVER_QUOTE_CANCELLED
