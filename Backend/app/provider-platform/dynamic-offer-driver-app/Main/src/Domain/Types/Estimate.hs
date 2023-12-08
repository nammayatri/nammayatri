{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Estimate where

import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as Variant
import qualified Kernel.Beam.Lib.UtilsTH as TH
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (encodeToText)

data Estimate = Estimate
  { id :: Id Estimate,
    requestId :: Id DSR.SearchRequest,
    vehicleVariant :: Variant.Variant,
    minFare :: Money,
    maxFare :: Money,
    estimateBreakupList :: [EstimateBreakup],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: WaitingCharges,
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic)

data WaitingCharges = WaitingCharges
  { waitingChargePerMin :: Maybe Money,
    waitingOrPickupCharges :: Maybe Money
  }
  deriving (Generic)

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    oldNightShiftCharge :: Centesimal, -- TODO: Doesn't make sense, to be removed
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic)

data EstimateBreakupD (s :: UsageSafety) = EstimateBreakup
  { title :: Text,
    price :: EstimateBreakupPriceD s
  }
  deriving stock (Show, Eq, Read, Ord, Generic)

type EstimateBreakup = EstimateBreakupD 'Safe

instance FromBackendRow Postgres [EstimateBreakupD 'Unsafe]

instance FromField [EstimateBreakupD 'Unsafe] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance {-# OVERLAPPING #-} TH.ToSQLObject (EstimateBreakupD 'Unsafe) where
  convertToSQLObject = TH.SQLObjectValue . show . encodeToText

instance FromField (EstimateBreakupD 'Unsafe) where
  fromField = fromFieldJSON

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [EstimateBreakupD 'Unsafe] where
  sqlValueSyntax unsafeEstimateBreakupList =
    let x = encodeToText <$> unsafeEstimateBreakupList
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [EstimateBreakupD 'Unsafe]

-- We shouldn't define JSON instances for 'Safe version
deriving instance FromJSON (EstimateBreakupD 'Unsafe)

deriving instance ToJSON (EstimateBreakupD 'Unsafe)

data EstimateBreakupPriceD (s :: UsageSafety) = EstimateBreakupPrice
  { currency :: Text,
    value :: Money
  }
  deriving stock (Generic, Show, Read, Eq, Ord)

type EstimateBreakupPrice = EstimateBreakupPriceD 'Safe

-- We shouldn't define JSON instances for 'Safe version
deriving instance FromJSON (EstimateBreakupPriceD 'Unsafe)

deriving instance ToJSON (EstimateBreakupPriceD 'Unsafe)
