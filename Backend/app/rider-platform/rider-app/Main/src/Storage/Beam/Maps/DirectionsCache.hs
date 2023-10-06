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

module Storage.Beam.Maps.DirectionsCache where

import Data.Aeson
import qualified Database.Beam as B
import Kernel.External.Maps (RouteInfo (..))
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data DirectionsCacheT f = DirectionsCacheT
  { id :: B.C f Text,
    originHash :: B.C f Text,
    destHash :: B.C f Text,
    slot :: B.C f Int,
    response :: B.C f RouteInfo,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DirectionsCacheT where
  data PrimaryKey DirectionsCacheT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DirectionsCache = DirectionsCacheT Identity

$(TH.enableKVPG ''DirectionsCacheT ['id] [['originHash], ['destHash]])

$(TH.mkTableInstances ''DirectionsCacheT "directions_cache")
