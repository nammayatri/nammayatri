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

module Storage.Beam.EstimateRevisedBreakup where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data EstimateRevisedBreakupT f = EstimateRevisedBreakupT
  { id :: B.C f Text,
    estimateRevisedId :: B.C f Text,
    title :: B.C f Text,
    priceCurrency :: B.C f Text,
    priceValue :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateRevisedBreakupT where
  data PrimaryKey EstimateRevisedBreakupT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type EstimateRevisedBreakup = EstimateRevisedBreakupT Identity

$(enableKVPG ''EstimateRevisedBreakupT ['id] [['estimateRevisedId]])

$(mkTableInstances ''EstimateRevisedBreakupT "estimate_revised_breakup")
