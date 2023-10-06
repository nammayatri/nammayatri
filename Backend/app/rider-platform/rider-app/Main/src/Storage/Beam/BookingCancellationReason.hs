{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.BookingCancellationReason where

import qualified Database.Beam as B
import qualified Domain.Types.BookingCancellationReason as Domain
import qualified Domain.Types.CancellationReason as DCR
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Tools.Beam.UtilsTH as TH

data BookingCancellationReasonT f = BookingCancellationReasonT
  { bookingId :: B.C f Text,
    rideId :: B.C f (Maybe Text),
    merchantId :: B.C f (Maybe Text),
    source :: B.C f Domain.CancellationSource,
    reasonStage :: B.C f (Maybe DCR.CancellationStage),
    reasonCode :: B.C f (Maybe DCR.CancellationReasonCode),
    additionalInfo :: B.C f (Maybe Text),
    driverCancellationLocationLat :: B.C f (Maybe Double),
    driverCancellationLocationLon :: B.C f (Maybe Double),
    driverDistToPickup :: B.C f (Maybe Meters)
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingCancellationReasonT where
  data PrimaryKey BookingCancellationReasonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . bookingId

type BookingCancellationReason = BookingCancellationReasonT Identity

$(TH.enableKVPG ''BookingCancellationReasonT ['bookingId] [])

$(TH.mkTableInstances ''BookingCancellationReasonT "booking_cancellation_reason")
