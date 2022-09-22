module RenderXml where

import Beckn.Prelude
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Utils.GenericPretty
import qualified Data.Map.Strict as Map
import Text.XML

type LocationUpdates = [[LatLong]]

data GpxDoc = GpxDoc
  { markers :: [GpxWaypoint],
    routes :: [GpxRoute],
    tracks :: [GpxTrack]
  }

data GpxWaypoint = GpxWaypoint
  { lat :: Double,
    lon :: Double,
    name :: Text,
    time :: Maybe UTCTime
  }
  deriving (Show, Eq, PrettyShow, Generic)

newtype GpxRoute = GpxRoute
  { points :: [GpxWaypoint]
  }
  deriving (Show, Eq, PrettyShow, Generic)

data GpxTrack = GpxTrack
  { segments :: [GpxSegment],
    name :: Text
  }
  deriving (Show, Eq, PrettyShow, Generic)

newtype GpxSegment = GpxSegment
  { points :: [GpxTrackPoint]
  }
  deriving (Show, Eq, PrettyShow, Generic)

data GpxTrackPoint = GpxTrackPoint
  { lat :: Double,
    lon :: Double,
    time :: Maybe UTCTime
  }
  deriving (Show, Eq, PrettyShow, Generic)

latLongToGpxTrackPoint :: LatLong -> GpxTrackPoint
latLongToGpxTrackPoint LatLong {..} =
  GpxTrackPoint
    { time = Nothing,
      ..
    }

latLongToGpxWaypoint :: Text -> LatLong -> GpxWaypoint
latLongToGpxWaypoint name LatLong {..} =
  GpxWaypoint
    { time = Nothing,
      ..
    }

batchesToGpxDoc :: Text -> LocationUpdates -> GpxDoc
batchesToGpxDoc routeTag lu = do
  let wp'Segments = flip map (zip [(1 :: Int) ..] lu) $ \(updNo, segm) ->
        (latLongToGpxWaypoint (routeTag <> ":batch" <> show updNo) $ head segm, GpxSegment $ map latLongToGpxTrackPoint segm)
      (wp', segments') = unzip wp'Segments
  GpxDoc
    { markers = wp',
      routes = [],
      tracks = [GpxTrack segments' routeTag]
    }

mergeGpxDocs :: (GpxWaypoint -> GpxWaypoint -> GpxWaypoint) -> GpxDoc -> GpxDoc -> GpxDoc
mergeGpxDocs mergeWpts doc1 doc2 =
  GpxDoc
    { routes = doc1.routes <> doc2.routes,
      tracks = doc1.tracks <> doc2.tracks,
      markers = zipWith mergeWpts doc1.markers doc2.markers
    }

mergeWithPointsOfFirst :: GpxDoc -> GpxDoc -> GpxDoc
mergeWithPointsOfFirst = mergeGpxDocs const

--

simplePrologue :: Prologue
simplePrologue = Prologue [] Nothing []

elementToDocument :: Element -> Document
elementToDocument el = Document simplePrologue el []

renderDoc :: GpxDoc -> Element
renderDoc doc =
  Element "gpx" Map.empty $
    map NodeElement $
      map (renderWaypoint "wpt") doc.markers
        <> map renderTrack doc.tracks
        <> map renderRoute doc.routes

renderWaypoint :: Name -> GpxWaypoint -> Element
renderWaypoint name wp = Element name attrs $ map NodeElement tags
  where
    attrs = renderGpsAttrs wp.lat wp.lon
    tags = catMaybes [Just $ dataElement "name" wp.name, dataElement "time" . show <$> wp.time]

renderRoute :: GpxRoute -> Element
renderRoute route = Element "rte" Map.empty $ map (NodeElement . renderWaypoint "rtept") route.points

dataElement :: Name -> Text -> Element
dataElement name data_ = Element name Map.empty [NodeContent data_]

renderGpsAttrs :: Double -> Double -> Map.Map Name Text
renderGpsAttrs lat lon = Map.fromList [("lat", show lat), ("lon", show lon)]

renderTrack :: GpxTrack -> Element
renderTrack t =
  Element "trk" Map.empty $
    map (NodeElement . renderSegment) t.segments
      <> [NodeElement $ dataElement "name" t.name]

renderSegment :: GpxSegment -> Element
renderSegment segm = Element "trkseg" Map.empty $ map (NodeElement . renderTrackPoint) segm.points

renderTrackPoint :: GpxTrackPoint -> Element
renderTrackPoint tp = Element "trkpt" (renderGpsAttrs tp.lat tp.lon) $ map NodeElement tags
  where
    tags = catMaybes [dataElement "time" . show <$> tp.time]
