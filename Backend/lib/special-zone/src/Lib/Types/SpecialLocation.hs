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
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.OpenApi hiding (name)
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.External.Maps (LatLong)
import Kernel.Prelude hiding (show)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Show

data RenderType
  = GATES_BASED
  | ZONE_BASED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''RenderType)

data PaymentMode
  = PAYTMEDC
  | CASH
  | ONLINE
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''PaymentMode)

-- | The payment mode applied when a special location does not specify any.
--   Single source of truth for the default used across upsert/CSV paths.
defaultPaymentModes :: [PaymentMode]
defaultPaymentModes = [CASH]

parsePaymentModes :: Maybe Text -> Either Text (Maybe [PaymentMode])
parsePaymentModes mbRaw =
  case mbRaw >>= cleanCell of
    Nothing -> Right Nothing
    Just raw ->
      case filter (not . T.null) (map (T.toUpper . T.strip) (T.splitOn "," raw)) of
        [] -> Right Nothing
        tokens -> Just <$> traverse parseToken tokens
  where
    cleanCell t = case T.strip t of "" -> Nothing; s -> Just s
    parseToken t = maybe (Left t) Right (readMaybe (T.unpack t))

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
    -- | Pickup/zone polygon as GeoJSON text (the @geom_geo_json@ column), used for
    --   in-Haskell point-in-polygon checks so the read path needs no PostGIS.
    geomGeoJson :: Maybe Text,
    linkedLocationsIds :: [Id SpecialLocation],
    locationType :: SpecialLocationType,
    enabled :: Bool,
    isOpenMarketEnabled :: Bool,
    isQueueEnabled :: Maybe Bool,
    enforceTollRoute :: Maybe Bool,
    render :: Maybe RenderType,
    priority :: Int,
    supportNumber :: Maybe Text,
    paymentModes :: Maybe [PaymentMode],
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
  | FixedRoutePickup
  | FixedRouteDrop
  deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''SpecialLocationType)

instance FromHttpApiData [SpecialLocationType] where
  parseUrlPiece = parseHeader . TEnc.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = BF.first T.pack . eitherDecode . LBS.fromStrict $ bs

instance ToHttpApiData [SpecialLocationType] where
  toUrlPiece = TEnc.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = LBS.toStrict . encode

data Area
  = Default
  | Pickup (Id SpecialLocation) (Maybe Text)
  | Drop (Id SpecialLocation)
  | PickupDrop (Id SpecialLocation) (Id SpecialLocation) (Maybe Text)
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
            ++ [ parsePickupWithGate r1
                 | r1 <- stripPrefix "Pickup_" r
               ]
            ++ [ (Drop (Id $ T.pack r1), "")
                 | r1 <- stripPrefix "Drop_" r
               ]
            ++ [ parsePickupDropWithGate r1
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
      parsePickupWithGate s =
        case splitOnSubstring "_Gate_" s of
          Just (slId, gateId) -> (Pickup (Id $ T.pack slId) (Just $ T.pack gateId), "")
          Nothing -> (Pickup (Id $ T.pack s) Nothing, "")
      parsePickupDropWithGate s =
        let (pickupId, rest1) = splitOnUnderscore s
         in case splitOnSubstring "_PGate_" rest1 of
              Just (dropId, pgate) -> (PickupDrop (Id $ T.pack pickupId) (Id $ T.pack dropId) (Just $ T.pack pgate), "")
              Nothing -> (PickupDrop (Id $ T.pack pickupId) (Id $ T.pack rest1) Nothing, "")
      splitOnSubstring :: String -> String -> Maybe (String, String)
      splitOnSubstring sep s = go [] s
        where
          sepLen = length sep
          go _ [] = Nothing
          go acc rest@(c : cs)
            | List.isPrefixOf sep rest = Just (reverse acc, List.drop sepLen rest)
            | otherwise = go (c : acc) cs

instance Show Area where
  show Default = "Default"
  show (Pickup specialLocationId mbGateId) = "Pickup_" <> T.unpack specialLocationId.getId <> maybe "" (\g -> "_Gate_" <> T.unpack g) mbGateId
  show (Drop specialLocationId) = "Drop_" <> T.unpack specialLocationId.getId
  show (PickupDrop pickupId dropId mbPGate) = "PickupDrop_" <> T.unpack pickupId.getId <> "_" <> T.unpack dropId.getId <> maybe "" (\pg -> "_PGate_" <> T.unpack pg) mbPGate

$(mkBeamInstancesForEnum ''Area)

instance ToParamSchema Area where
  toParamSchema _ =
    mempty
      & title ?~ "Area"
      & type_ ?~ OpenApiString
      & format ?~ "Default,Pickup_<SpecialLocationId>[_Gate_<GateId>],Drop_<SpecialLocationId>,PickupDrop_<PickupSpecialLocationId>_<DropSpecialLocationId>[_PGate_<GateId>]"

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

areaToText :: Area -> Text
areaToText = T.pack . show

parsePickupDropFromText :: Text -> Maybe Area
parsePickupDropFromText t =
  case readMaybe (unpack t) of
    Just a@(PickupDrop {}) -> Just a
    _ -> Nothing

-- | Strip gate IDs from an Area, returning the base area without gate info.
-- Used for fallback: try gate-specific fare product first, then fall back to this.
stripGateId :: Area -> Area
stripGateId (Pickup slId _) = Pickup slId Nothing
stripGateId (Drop slId) = Drop slId
stripGateId (PickupDrop pId dId _) = PickupDrop pId dId Nothing
stripGateId Default = Default

-- | Check if an Area has any gate ID set.
hasGateId :: Area -> Bool
hasGateId (Pickup _ (Just _)) = True
hasGateId (PickupDrop _ _ (Just _)) = True
hasGateId _ = False

-- | Extract the pickup gate ID from an Area, if present.
pickupGateIdFromArea :: Area -> Maybe Text
pickupGateIdFromArea (Pickup _ mbGateId) = mbGateId
pickupGateIdFromArea (PickupDrop _ _ mbGateId) = mbGateId
pickupGateIdFromArea _ = Nothing

pickupSpecialZoneIdFromArea :: Area -> Maybe Text
pickupSpecialZoneIdFromArea (Pickup slId _) = Just slId.getId
pickupSpecialZoneIdFromArea (Drop _) = Nothing
pickupSpecialZoneIdFromArea (PickupDrop slId _ _) = Just slId.getId
pickupSpecialZoneIdFromArea Default = Nothing
