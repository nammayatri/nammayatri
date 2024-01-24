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
{-# OPTIONS_GHC -Wwarn=orphans #-}

module Storage.Beam.SearchTry where

import qualified Database.Beam as B
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.SearchTry as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data SearchTryT f = SearchTryT
  { id :: B.C f Text,
    messageId :: B.C f Text,
    requestId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text),
    startTime :: B.C f UTCTime,
    validTill :: B.C f UTCTime,
    estimateId :: B.C f Text,
    baseFare :: B.C f Money,
    customerExtraFee :: B.C f (Maybe Money),
    status :: B.C f Domain.SearchTryStatus,
    vehicleVariant :: B.C f Variant.Variant,
    searchRepeatCounter :: B.C f Int,
    tripCategory :: B.C f (Maybe DTC.TripCategory),
    searchRepeatType :: B.C f Domain.SearchRepeatType,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchTryT where
  data PrimaryKey SearchTryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchTry = SearchTryT Identity

$(enableKVPG ''SearchTryT ['id] [['requestId]])

$(mkTableInstances ''SearchTryT "search_try")
