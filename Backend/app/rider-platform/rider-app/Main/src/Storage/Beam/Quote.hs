{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Quote where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy.FareProductType as Domain
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Version
import Tools.Beam.UtilsTH

data QuoteT f = QuoteT
  { id :: B.C f Text,
    fareProductType :: B.C f Domain.FareProductType,
    requestId :: B.C f Text,
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    currency :: B.C f (Maybe Currency),
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    itemId :: B.C f Text,
    distanceToNearestDriver :: B.C f (Maybe HighPrecMeters),
    distanceToNearestDriverValue :: B.C f (Maybe HighPrecDistance),
    distanceUnit :: B.C f (Maybe DistanceUnit),
    vehicleVariant :: B.C f DVST.VehicleServiceTierType,
    serviceTierName :: B.C f (Maybe Text),
    serviceTierShortDesc :: B.C f (Maybe Text),
    tripTermsId :: B.C f (Maybe Text),
    rentalDetailsId :: B.C f (Maybe Text),
    driverOfferId :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f (Maybe Text),
    specialZoneQuoteId :: B.C f (Maybe Text),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    validTill :: B.C f UTCTime,
    clientBundleVersion :: B.C f (Maybe Text),
    clientSdkVersion :: B.C f (Maybe Text),
    clientConfigVersion :: B.C f (Maybe Text),
    clientOsType :: B.C f (Maybe DeviceType),
    clientOsVersion :: B.C f (Maybe Text),
    backendConfigVersion :: B.C f (Maybe Text),
    backendAppVersion :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteT where
  data PrimaryKey QuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Quote = QuoteT Identity

$(enableKVPG ''QuoteT ['id] [['requestId], ['driverOfferId]])

$(mkTableInstances ''QuoteT "quote")
