module Helpers.SpecialZoneAndHotSpots where

import Prelude
import Screens.Types
import Services.API as SA
import Screens.HomeScreen.ScreenData as HomeScreenData
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore, isLocalStageOn)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Types.App (FlowBT)
import JBridge
import Data.Lens ((^.))
import Helpers.Utils
import Data.String as DS
import Data.Array as DA
import ModifyScreenState (modifyScreenState)
import Types.App (ScreenType(..))
import Engineering.Helpers.BackTrack (liftFlowBT)
import Accessor (_lat, _lon)
import Common.Types.Config as CCT
import Common.DefaultConfig as CC
import Data.Argonaut.Decode.Class as AD
import Effect.Uncurried (runEffectFn1)
import Data.Argonaut.Decode.Parser as ADP
import Common.Resources.Constants (zoomLevel)
import Data.Either (Either(..))
import Engineering.Helpers.Commons as EHC
import Data.Argonaut.Core as AC
import Data.Argonaut.Encode.Class as AE
import Data.Function.Uncurried (runFn3, runFn2)
import Engineering.Helpers.GeoHash as EHG
import Data.Map as DM
import Common.Types.App
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color

filterHotSpots :: HomeScreenState -> Array SA.HotSpotInfo -> Number -> Number -> Array Location
filterHotSpots state hotSpots lat lon =
  if DA.null hotSpots then []
  else
    let hotSpotInfo = transformHotSpotInfo $ DA.take 3 (DA.filter (\(SA.HotSpotInfo hotSpot) ->
                                                                  let (SA.LatLong point) = hotSpot.centroidLatLong
                                                                      distance = (getDistanceBwCordinates lat lon point.lat point.lon) * 1000.0
                                                                  in
                                                                    distance < state.data.config.mapConfig.locateOnMapConfig.hotSpotConfig.showHotSpotsWithinRadius
                                                                ) hotSpots)
        selectedSpot = case state.props.hotSpot.selectedSpot of
                          Just spot -> [spot]
                          Nothing -> []
        points = DA.nub $ (map (\item -> {  place: "",
                                            lat  : item.lat,
                                            lng : item.lon,
                                            address : Nothing,
                                            city : Nothing,
                                            isSpecialPickUp : Just false
                                          }) hotSpotInfo) <> selectedSpot
    in points

mapSpecialZoneGates :: Array SA.GateInfoFull -> Array Location
mapSpecialZoneGates gates = 
  map (\(SA.GateInfoFull item) -> { place: item.name,
                              lat  : (item.point)^._lat,
                              lng : (item.point)^._lon,
                              address : if item.address == Just "NULL"
                                          then Just item.name
                                          else item.address,
                              city : Nothing,
                              isSpecialPickUp : Just $ isJust item.geoJson
                            }) gates

getZoneType :: String -> ZoneType
getZoneType tag =
  case tag of
    "SureMetro" -> METRO
    "SureWarriorMetro" -> METRO
    "SureBlockedAreaForAutos" -> AUTO_BLOCKED
    "PickupZone" -> SPECIAL_PICKUP
    "SureShoppingMall" -> SHOPPING_MALL
    "SureAirport" -> AIRPORT
    _ -> NOZONE

getSpecialTag :: Maybe String -> SpecialTags
getSpecialTag specialTag =
  case specialTag of
    Just tag ->
      let zones = DS.split (DS.Pattern "_") tag
          pickupZone = getZoneType $ fromMaybe "" (zones DA.!! 3)
          sourceTag = if pickupZone == SPECIAL_PICKUP then SPECIAL_PICKUP else getZoneType $ fromMaybe "" (DA.head zones)
          destinationTag = getZoneType $ fromMaybe "" (zones DA.!! 1)
          priorityTag = if zones DA.!! 2 == Just "PriorityPickup" then sourceTag else destinationTag
      in { sourceTag : sourceTag, destinationTag : destinationTag, priorityTag : priorityTag}
    Nothing -> HomeScreenData.dummyZoneType

getConfirmLocationCategory :: HomeScreenState -> ZoneType
getConfirmLocationCategory state = 
  if state.props.locateOnMapProps.isSpecialPickUpGate then 
    SPECIAL_PICKUP 
  else if isJust state.props.hotSpot.centroidPoint then
    HOTSPOT (isJust state.props.hotSpot.selectedSpot)
  else 
    state.props.confirmLocationCategory

transformHotSpotInfo :: Array SA.HotSpotInfo -> Array HotSpotData
transformHotSpotInfo hotSpotInfo = map (\hotSpot -> transformHotSpot hotSpot) hotSpotInfo

transformHotSpot :: SA.HotSpotInfo -> HotSpotData
transformHotSpot (SA.HotSpotInfo hotSpot) =
  let (SA.LatLong centroidLatLong) = hotSpot.centroidLatLong
  in { lat : centroidLatLong.lat, lon : centroidLatLong.lon }

checkSpecialPickupZone :: Maybe String -> Boolean
checkSpecialPickupZone maybeLabel = 
  case maybeLabel of
    Just label -> let arr = DS.split (DS.Pattern "_") label
                      specialPickupZone = fromMaybe "" (arr DA.!! 3)
                  in specialPickupZone == "PickupZone"
    Nothing    -> false

decodeGeoJson :: String -> Maybe CCT.GeoJson
decodeGeoJson stringGeoJson = 
  case (AD.decodeJson =<< ADP.parseJson stringGeoJson) of
    Right resp -> Just resp
    Left err   -> Nothing

specialZoneTagConfig :: ZoneType -> SpecialZoneTagConfig
specialZoneTagConfig zoneType =
  case zoneType of
    SPECIAL_PICKUP -> 
      { icon : "ny_ic_location_pin_white"
      , text : if isLocalStageOn ConfirmingLocation then getString SPECIAL_PICKUP_ZONE else getString SPECIAL_PICKUP_ZONE_RIDE
      , infoPopUpConfig : Just $ { title : getString SPECIAL_PICKUP_ZONE
                                 , primaryText : getString WE_WILL_TRY_TO_CONNECT_YOU_WITH_DRIVER_IN_CLOSEST_PICKUP_ZONE
                                 , secondaryText : getString THIS_PROVIDES_YOU_AN_INSTANT_PICKUP_EXPERIENCE
                                 , primaryButtonText : getString GOT_IT
                                 , icon : "ny_ic_sp_pickup_zone_map" }
      , backgroundColor : Color.green900
      }
    AUTO_BLOCKED -> 
      { icon : "ny_ic_zone_walk"
      , text : getString GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }
    METRO ->
      { icon : "ny_ic_metro_white"
      , text : getString METRO_RIDE
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }
    HOTSPOT onSpot ->
      { icon : if onSpot then "ny_ic_zone_walk" else "ny_ic_select_spot"
      , text : getString $ if onSpot then GO_TO_SELECTED_SPOT_FOR_PICKUP else SELECT_POPULAR_SPOT_FOR_HASSLE_FREE_PICKUP
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }
    _ ->
      { icon : "ny_ic_zone_walk"
      , text : getString GO_TO_SELECTED_SPOT_FOR_PICKUP
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }

zoneLabelIcon :: ZoneType -> String
zoneLabelIcon zoneType =
  case zoneType of
    METRO -> "ny_ic_metro_black"
    _ -> ""

findSpecialPickupZone :: Maybe String -> Maybe (Array Location) -> Number -> Number -> Maybe SpecialLocationList
findSpecialPickupZone stringGeoJson gates lat lon =
  case stringGeoJson, gates of
    Just stringGeoJson', Just gates' -> 
      case decodeGeoJson stringGeoJson' of
        Just geoJson -> 
          let gate = DA.find (\gate -> gate.lat == lat && gate.lng == lon) gates'
          in case gate of
                Just gate' -> 
                  let feature = DA.find (\feature -> feature.properties.name == gate'.place) geoJson.features
                  in Just { geoJson : AC.stringify $ AE.encodeJson geoJson{ features = [feature] }, gates : [gate'], locationName : "", category : "", city : "" }
                Nothing -> Nothing
        Nothing -> Nothing
    _, _ -> Nothing

transformGeoJsonFeature :: Maybe String -> Array SA.GateInfoFull -> String
transformGeoJsonFeature geoJson gateInfoFulls =
  if DS.null (fromMaybe "" geoJson) && DA.null gateInfoFulls then ""
  else
    AC.stringify $ AE.encodeJson CC.defaultGeoJson { features = geoJsonFeatures }
    where
      geoJsonFeatures :: Array CCT.GeoJsonFeature
      geoJsonFeatures = 
        DA.foldr (\(SA.GateInfoFull gateInfoFull) specialZones -> 
                  case gateInfoFull.geoJson of
                    Just _ -> specialZones <> [createGeoJsonFeature (SA.GateInfoFull gateInfoFull)]
                    Nothing -> specialZones
                 ) [] gateInfoFulls
        <> case geoJson of
              Just geoJson' -> DA.singleton CC.defaultGeoJsonFeature{ geometry = fromMaybe CC.defaultGeoJsonGeometry (decodeGeoJsonGeometry geoJson') }
              Nothing -> []

      decodeGeoJsonGeometry :: String -> Maybe CCT.GeoJsonGeometry
      decodeGeoJsonGeometry stringGeometry =
        case (AD.decodeJson =<< ADP.parseJson stringGeometry) of
          Right resp -> Just resp
          Left err   -> Nothing
      
      createGeoJsonFeature :: SA.GateInfoFull -> CCT.GeoJsonFeature
      createGeoJsonFeature (SA.GateInfoFull gateInfoFull) = 
        CC.defaultGeoJsonFeature {
            properties {
                name = gateInfoFull.name
              , defaultDriverExtra = fromMaybe 0 gateInfoFull.defaultDriverExtra
              , canQueueUpOnGate = fromMaybe false gateInfoFull.canQueueUpOnGate
            }
          , geometry = case gateInfoFull.geoJson of
                          Just geoJson -> fromMaybe CC.defaultGeoJsonGeometry (decodeGeoJsonGeometry geoJson)
                          Nothing      -> CC.defaultGeoJsonGeometry
        }