{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FareProduct where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Types.FarePolicy as FarePolicyD
import Domain.Types.Merchant
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Lib.Types.SpecialLocation (SpecialLocation (..))
import qualified Text.Show
import qualified Tools.Beam.UtilsTH as TH

data FlowType
  = RIDE_OTP
  | NORMAL
  deriving (Show, Read, Generic, Eq, ToSchema, FromJSON, ToJSON, Ord)

data Area
  = Pickup (Id SpecialLocation)
  | Drop (Id SpecialLocation)
  | Default
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable Area

instance Read Area where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (Default, r1)
            | r1 <- stripPrefix "Default" r
          ]
            ++ [ (Pickup (Id $ T.pack r1), "")
                 | r1 <- stripPrefix "Pickup_" r
               ]
            ++ [ (Drop (Id $ T.pack r1), "")
                 | r1 <- stripPrefix "Drop_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance Show Area where
  show (Pickup specialLocationId) = "Pickup_" <> T.unpack specialLocationId.getId
  show (Drop specialLocationId) = "Drop_" <> T.unpack specialLocationId.getId
  show Default = "Default"

data FareProduct = FareProduct
  { id :: Id FareProduct,
    merchantId :: Id Merchant,
    farePolicyId :: Id FarePolicyD.FarePolicy,
    vehicleVariant :: Variant.Variant,
    area :: Area,
    flow :: FlowType
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON, ToJSON)

$(TH.mkBeamInstancesForEnum ''FlowType)

$(TH.mkBeamInstancesForEnum ''Area)
