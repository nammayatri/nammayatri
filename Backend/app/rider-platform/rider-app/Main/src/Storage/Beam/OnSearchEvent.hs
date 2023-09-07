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

module Storage.Beam.OnSearchEvent where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data OnSearchEventT f = OnSearchEventT
  { id :: B.C f Text,
    bppId :: B.C f Text,
    messageId :: B.C f Text,
    errorCode :: B.C f (Maybe Text),
    errorType :: B.C f (Maybe Text),
    errorMessage :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OnSearchEventT where
  data PrimaryKey OnSearchEventT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type OnSearchEvent = OnSearchEventT Identity

$(enableKVPG ''OnSearchEventT ['id] [])

$(mkTableInstances ''OnSearchEventT "on_search_event")
