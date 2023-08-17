{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SpecialZoneQuote where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data SpecialZoneQuoteT f = SpecialZoneQuoteT
  { id :: B.C f Text,
    quoteId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table SpecialZoneQuoteT where
  data PrimaryKey SpecialZoneQuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SpecialZoneQuote = SpecialZoneQuoteT Identity

specialZoneQuoteTMod :: SpecialZoneQuoteT (B.FieldModification (B.TableField SpecialZoneQuoteT))
specialZoneQuoteTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      quoteId = B.fieldNamed "quote_id"
    }

$(enableKVPG ''SpecialZoneQuoteT ['id] [])

$(mkTableInstances ''SpecialZoneQuoteT "special_zone_quote" "atlas_app")
