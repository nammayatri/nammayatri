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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SpecialZoneQuote where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
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

instance ModelMeta SpecialZoneQuoteT where
  modelFieldModification = specialZoneQuoteTMod
  modelTableName = "special_zone_quote"
  modelSchemaName = Just "atlas_app"

type SpecialZoneQuote = SpecialZoneQuoteT Identity

instance FromJSON SpecialZoneQuote where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SpecialZoneQuote where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SpecialZoneQuote

specialZoneQuoteTMod :: SpecialZoneQuoteT (B.FieldModification (B.TableField SpecialZoneQuoteT))
specialZoneQuoteTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      quoteId = B.fieldNamed "quote_id"
    }

instance Serialize SpecialZoneQuote where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

specialZoneQuoteToHSModifiers :: M.Map Text (A.Value -> A.Value)
specialZoneQuoteToHSModifiers =
  M.empty

specialZoneQuoteToPSModifiers :: M.Map Text (A.Value -> A.Value)
specialZoneQuoteToPSModifiers =
  M.empty

$(enableKVPG ''SpecialZoneQuoteT ['id] [])
