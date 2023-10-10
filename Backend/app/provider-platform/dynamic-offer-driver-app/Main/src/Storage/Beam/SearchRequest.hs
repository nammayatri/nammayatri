{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.SearchRequest where

import qualified Database.Beam as B
import qualified Domain.Types.FareProduct as FareProductD
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH
import qualified Tools.Maps as Maps

data SearchRequestT f = SearchRequestT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    providerId :: B.C f Text,
    fromLocationId :: B.C f (Maybe Text),
    toLocationId :: B.C f (Maybe Text),
    area :: B.C f (Maybe FareProductD.Area),
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    bapCity :: B.C f (Maybe Context.City),
    bapCountry :: B.C f (Maybe Context.Country),
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    customerLanguage :: B.C f (Maybe Maps.Language),
    disabilityTag :: B.C f (Maybe Text),
    device :: B.C f (Maybe Text),
    autoAssignEnabled :: B.C f (Maybe Bool),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    tag :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequest = SearchRequestT Identity

$(enableKVPG ''SearchRequestT ['id] [['transactionId]])

$(mkTableInstances ''SearchRequestT "search_request")
