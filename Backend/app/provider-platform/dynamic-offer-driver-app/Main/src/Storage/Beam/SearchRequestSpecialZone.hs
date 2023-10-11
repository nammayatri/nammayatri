{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}

module Storage.Beam.SearchRequestSpecialZone where

import qualified Database.Beam as B
import qualified Domain.Types.FareProduct as FareProductD
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data SearchRequestSpecialZoneT f = SearchRequestSpecialZoneT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    messageId :: B.C f Text,
    startTime :: B.C f UTCTime,
    validTill :: B.C f UTCTime,
    providerId :: B.C f Text,
    fromLocationId :: B.C f (Maybe Text),
    toLocationId :: B.C f (Maybe Text),
    area :: B.C f (Maybe FareProductD.Area),
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestSpecialZoneT where
  data PrimaryKey SearchRequestSpecialZoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequestSpecialZone = SearchRequestSpecialZoneT Identity

$(enableKVPG ''SearchRequestSpecialZoneT ['id] [['transactionId], ['messageId]])

$(mkTableInstances ''SearchRequestSpecialZoneT "search_request_special_zone")
