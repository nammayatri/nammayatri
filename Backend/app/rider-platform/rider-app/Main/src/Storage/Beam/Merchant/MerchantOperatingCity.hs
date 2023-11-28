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

module Storage.Beam.Merchant.MerchantOperatingCity where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Tools.Beam.UtilsTH

data MerchantOperatingCityT f = MerchantOperatingCityT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    merchantShortId :: B.C f Text,
    city :: B.C f Context.City,
    contextCity :: B.C f Context.City
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantOperatingCityT where
  data PrimaryKey MerchantOperatingCityT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantOperatingCity = MerchantOperatingCityT Identity

$(enableKVPG ''MerchantOperatingCityT ['id] [])

$(mkTableInstances ''MerchantOperatingCityT "merchant_operating_city")
