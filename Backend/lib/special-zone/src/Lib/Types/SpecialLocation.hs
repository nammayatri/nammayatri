{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Lib.Types.SpecialLocation where

import Control.Lens.Operators
import Data.Aeson
import qualified Data.List as List
import Data.OpenApi hiding (name)
import Data.Text (unpack)
import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.External.Maps (LatLong)
import Kernel.Prelude hiding (show)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Show
import Kernel.Utils.TH

data Merchant

data MerchantOperatingCity

data SpecialLocation = SpecialLocation
  { id :: Id SpecialLocation,
    locationName :: Text,
    category :: Text,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    gates :: [GatesInfo], --TODO: deprecate this later
    geom :: Maybe Text,
    linkedLocationsIds :: [Id SpecialLocation],
    locationType :: SpecialLocationType,
    enabled :: Bool,
    priority :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data GatesInfo = GatesInfo
  { point :: LatLong,
    name :: Text,
    address :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data SpecialLocationType
  = Open
  | Closed
  | FixedRoute -- Areas without geometry for fixed route pricing
  deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''SpecialLocationType)

data Area
  = Pickup (Id SpecialLocation)
  | Drop (Id SpecialLocation)
  | PickupDrop (Id SpecialLocation) (Id SpecialLocation)
  | Default
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToSchema, ToJSON)
  deriving (PrettyShow) via Showable Area

instance FromJSON Area where
  parseJSON (String val) = case readMaybe (unpack val) of
    Just a -> return a
    Nothing -> fail "Not able to parse string to Area"
  parseJSON other = genericParseJSON defaultOptions other

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
            ++ [ ( let (pickupId, dropId) = splitOnUnderscore r1
                    in (PickupDrop (Id $ T.pack pickupId) (Id $ T.pack dropId), "")
                 )
                 | r1 <- stripPrefix "PickupDrop_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r
      splitOnUnderscore s =
        let (x, rest) = break (== '_') s
         in case rest of
              ('_' : y) -> (x, y)
              _ -> (x, "")

instance Show Area where
  show (Pickup specialLocationId) = "Pickup_" <> T.unpack specialLocationId.getId
  show (Drop specialLocationId) = "Drop_" <> T.unpack specialLocationId.getId
  show (PickupDrop pickupId dropId) = "PickupDrop_" <> T.unpack pickupId.getId <> "_" <> T.unpack dropId.getId
  show Default = "Default"

$(mkBeamInstancesForEnum ''Area)

instance ToParamSchema Area where
  toParamSchema _ =
    mempty
      & title ?~ "Area"
      & type_ ?~ OpenApiString
      & format ?~ "Default,Pickup_<SpecialLocationId>,Drop_<SpecialLocationId>,PickupDrop_<PickupSpecialLocationId>_<DropSpecialLocationId>"

instance FromHttpApiData Area where
  parseUrlPiece = parse . T.unpack
    where
      parse "Default" = Right Default
      parse str =
        case readMaybe str :: Maybe Area of
          Just area -> Right area
          Nothing -> Right Default

instance ToHttpApiData Area where
  toUrlPiece = T.pack . show
