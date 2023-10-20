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
import qualified Domain.Types.VehicleVariant as VehVar
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data QuoteT f = QuoteT
  { id :: B.C f Text,
    fareProductType :: B.C f Domain.FareProductType,
    requestId :: B.C f Text,
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    providerName :: B.C f Text,
    itemId :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    providerCompletedRidesCount :: B.C f Int,
    distanceToNearestDriver :: B.C f (Maybe HighPrecMeters),
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    tripTermsId :: B.C f (Maybe Text),
    rentalDetailsId :: B.C f (Maybe Text),
    driverOfferId :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    specialZoneQuoteId :: B.C f (Maybe Text),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime
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
