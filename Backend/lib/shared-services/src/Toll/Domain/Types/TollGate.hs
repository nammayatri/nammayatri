module Toll.Domain.Types.TollGate
  ( TollGate (..),
    lineStringToSegments,
    geoPolygonToLatLongRings,
    lineSegmentToLineString,
    lineStringToEndpoints,
    lineStringToGeoJsonText,
    geoPolygonToText,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Foldable
import Data.Geospatial.Internal.BasicTypes (GeoPositionWithoutCRS (..), PointXY (..), retrieveXY)
import Data.Geospatial.Internal.Geometry.GeoLine (GeoLine (..))
import Data.Geospatial.Internal.Geometry.GeoPolygon (GeoPolygon (..))
import Data.LineString (LineString, fromLineString, lineStringHead, lineStringLast, makeLineString)
import qualified Data.LinearRing as LinearRing
import Data.OpenApi hiding (name)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.App ()
import Kernel.Utils.ComputeIntersection (LineSegment (..))

data TollGate
  = LineGate (LineString GeoPositionWithoutCRS)
  | PolyGate GeoPolygon
  deriving stock (Eq, Generic, Show)

latLongToGeoPosition :: LatLong -> GeoPositionWithoutCRS
latLongToGeoPosition (LatLong lat lon) = GeoPointXY $ PointXY lon lat

geoPositionToLatLong :: GeoPositionWithoutCRS -> Maybe LatLong
geoPositionToLatLong GeoEmpty = Nothing
geoPositionToLatLong pos =
  let PointXY lon lat = retrieveXY pos
   in Just $ LatLong lat lon

lineSegmentToLineString :: LineSegment -> LineString GeoPositionWithoutCRS
lineSegmentToLineString (LineSegment (LatLong lat1 lon1) (LatLong lat2 lon2)) =
  makeLineString (latLongToGeoPosition $ LatLong lat1 lon1) (latLongToGeoPosition $ LatLong lat2 lon2) mempty

lineStringToSegments :: LineString GeoPositionWithoutCRS -> [LineSegment]
lineStringToSegments ls =
  let points = mapMaybe geoPositionToLatLong $ fromLineString ls
   in zipWith LineSegment points (drop 1 points)

lineStringToEndpoints :: LineString GeoPositionWithoutCRS -> Maybe LineSegment
lineStringToEndpoints ls = do
  start <- geoPositionToLatLong $ lineStringHead ls
  end <- geoPositionToLatLong $ lineStringLast ls
  pure $ LineSegment start end

geoPolygonToLatLongRings :: GeoPolygon -> [[LatLong]]
geoPolygonToLatLongRings (GeoPolygon rings) =
  map linearRingToLatLongs $ Foldable.toList rings
  where
    linearRingToLatLongs ring =
      mapMaybe geoPositionToLatLong $ LinearRing.fromLinearRing ring

lineStringToGeoJsonText :: LineString GeoPositionWithoutCRS -> Text
lineStringToGeoJsonText ls =
  TE.decodeUtf8 $ BSL.toStrict $ Aeson.encode (GeoLine ls)

decodeLineStringFromRead :: String -> [(LineString GeoPositionWithoutCRS, String)]
decodeLineStringFromRead s =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 $ T.pack s) of
    Right (GeoLine ls) -> [(ls, "")]
    Left _ ->
      case readMaybe s :: Maybe LineSegment of
        Just seg -> [(lineSegmentToLineString seg, "")]
        Nothing ->
          case Aeson.eitherDecodeStrict (C8.pack s) of
            Right (GeoLine ls) -> [(ls, "")]
            _ -> []

decodeGeoPolygonFromRead :: String -> [(GeoPolygon, String)]
decodeGeoPolygonFromRead s =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 $ T.pack s) of
    Right gp -> [(gp, "")]
    Left _ ->
      case Aeson.eitherDecodeStrict (C8.pack s) of
        Right gp -> [(gp, "")]
        Left _ ->
          case readMaybe s :: Maybe Text of
            Just txt ->
              case Aeson.eitherDecodeStrict (TE.encodeUtf8 txt) of
                Right gp -> [(gp, "")]
                _ -> []
            Nothing -> []

instance ToSchema TollGate where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "TollGate") $ toInlinedSchema (Proxy :: Proxy Aeson.Value)

geoPolygonToText :: GeoPolygon -> Text
geoPolygonToText =
  TE.decodeUtf8 . BSL.toStrict . Aeson.encode

instance Aeson.ToJSON TollGate where
  toJSON (LineGate ls) = Aeson.toJSON $ GeoLine ls
  toJSON (PolyGate gp) = Aeson.toJSON gp

instance Aeson.FromJSON TollGate where
  parseJSON v =
    parseGeoLineGate v
      <|> parseLegacyLineSegmentGate v
      <|> parseGeoPolygonGate v
    where
      parseGeoLineGate val = do
        GeoLine ls <- Aeson.parseJSON val
        pure $ LineGate ls

      parseLegacyLineSegmentGate val = do
        seg <- Aeson.parseJSON val
        pure $ LineGate $ lineSegmentToLineString (seg :: LineSegment)

      parseGeoPolygonGate val = do
        gp <- Aeson.parseJSON val
        pure $ PolyGate (gp :: GeoPolygon)

parseTollGateFromDbText :: BS.ByteString -> Maybe TollGate
parseTollGateFromDbText bs =
  case Aeson.decodeStrict bs of
    Just gate -> Just gate
    Nothing ->
      (\seg -> LineGate $ lineSegmentToLineString seg) <$> readMaybe (C8.unpack bs)

tollGateToDbText :: TollGate -> Text
tollGateToDbText gate =
  TE.decodeUtf8 $ BSL.toStrict $ Aeson.encode (Aeson.toJSON gate)

instance Ord TollGate where
  compare = comparing tollGateToDbText

instance Read TollGate where
  readsPrec p = readParen (p > 10) $ \s ->
    case lex s of
      ("LineGate", rest) : _ ->
        [ (LineGate ls, t)
          | (ls, t) <- decodeLineStringFromRead rest
        ]
          ++ [ (LineGate (lineSegmentToLineString seg), t)
               | (seg, t) <- readsPrec 11 rest :: [(LineSegment, String)]
             ]
      ("PolyGate", rest) : _ ->
        [ (PolyGate gp, t)
          | (gp, t) <- decodeGeoPolygonFromRead rest
        ]
          ++ [ (PolyGate gp, t)
               | (txt, t) <- readsPrec 11 rest :: [(Text, String)],
                 (gp, _) <- decodeGeoPolygonFromRead (T.unpack txt)
             ]
      _ -> maybe [] (\g -> [(g, "")]) $ parseTollGateFromDbText (C8.pack s)

instance FromField TollGate where
  fromField f mbValue =
    case mbValue of
      Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
      Just bs ->
        case parseTollGateFromDbText bs of
          Just gate -> pure gate
          Nothing ->
            DPSF.returnError DPSF.ConversionFailed f $
              "Could not parse TollGate from DB value: "
                <> show (C8.unpack bs)

instance HasSqlValueSyntax be Aeson.Value => HasSqlValueSyntax be TollGate where
  sqlValueSyntax = sqlValueSyntax . Aeson.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be TollGate

instance FromBackendRow Postgres TollGate

instance FromField [TollGate] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [TollGate] where
  sqlValueSyntax batchList =
    sqlValueSyntax (V.fromList $ tollGateToDbText <$> batchList)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [TollGate]

instance FromBackendRow Postgres [TollGate]
