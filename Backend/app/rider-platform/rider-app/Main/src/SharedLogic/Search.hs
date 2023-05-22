module SharedLogic.Search where

import Data.Aeson
import Data.OpenApi hiding (Header)
import qualified Data.OpenApi as OpenApi hiding (Header)
import Kernel.Prelude
import Kernel.Types.Id
import qualified SharedLogic.Search.OneWay as DOneWaySearch
import qualified SharedLogic.Search.Rental as DRentalSearch
import SharedLogic.Types.SearchRequest (SearchRequest)
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps

data SearchReq = OneWaySearch DOneWaySearch.OneWaySearchReq | RentalSearch DRentalSearch.RentalSearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where
  toJSON = genericToJSON fareProductOptions

instance FromJSON SearchReq where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema SearchReq where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = J.fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

data SearchRes = SearchRes
  { searchId :: Id SearchRequest,
    searchExpiry :: UTCTime,
    routeInfo :: Maybe Maps.RouteInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  x -> x

getLongestRouteDistance :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getLongestRouteDistance [] = Nothing
getLongestRouteDistance (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getLongestRouteDistance routeInfoArray
      Just $ comparator' routeInfo restRouteresult
  where
    comparator' route1 route2 =
      if route1.distance > route2.distance
        then route1
        else route2

getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteresult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteresult
getRouteInfoWithShortestDuration [] = Nothing

comparator :: Maps.RouteInfo -> Maps.RouteInfo -> Maps.RouteInfo
comparator route1 route2 =
  if route1.duration < route2.duration
    then route1
    else route2
