{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Merchant.Overlay where

import Data.Aeson
import qualified Database.Beam as B

import GHC.Generics (Generic)
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import qualified Tools.Beam.UtilsTH as TH

data OverlayT f = OverlayT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    overlayKey :: B.C f Text,
    language :: B.C f Language,
    udf1 :: B.C f (Maybe Text),
    title :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    imageUrl :: B.C f (Maybe Text),
    okButtonText :: B.C f (Maybe Text),
    cancelButtonText :: B.C f (Maybe Text),
    actions :: B.C f [Text],
    link :: B.C f (Maybe Text),
    method :: B.C f (Maybe Text),
    reqBody :: B.C f Value,
    endPoint :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table OverlayT where
  data PrimaryKey OverlayT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Overlay = OverlayT Identity

$(TH.enableKVPG ''OverlayT ['id] [['merchantId, 'overlayKey]])

$(TH.mkTableInstances ''OverlayT "merchant_overlay")
