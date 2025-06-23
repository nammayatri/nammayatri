module Screens.MetroWarriorsScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Effect (Effect)
import Prelude
import PrestoDOM
import Screens.MetroWarriorsScreen.Controller
import Screens.Types as ST
import JBridge (addCarousel)
import Data.Function.Uncurried (runFn2)
import Screens.MetroWarriorsScreen.ComponentConfig
import Helpers.Utils as HU
import Components.GenericHeader as GenericHeader
import Components.SwitchButtonView as SwitchButtonView
import Control.Monad.Trans.Class (lift)
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App as CTA
import Mobility.Prelude as MP
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.Array as DA
import Data.String as DS
import PrestoDOM.List as PList
import Presto.Core.Types.Language.Flow (Flow)
import Types.App (FlowBT, GlobalState(..), defaultGlobalState)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.BackTrack (liftFlowBT)
import Data.Maybe as Mb
import Effect.Aff (launchAff)
import Screens.MetroWarriorsScreen.ScreenData
import Helpers.API as HelpersAPI
import Data.Either
import Services.API as API
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Storage (getValueToLocalStore, KeyStore(..), setValueToLocalStore)
import DecodeUtil
import Foreign (Foreign)
import Control.Monad.Except (runExcept)
import Foreign.Generic (decode)
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Maybe (Maybe(..))

screen :: ST.MetroWarriorsScreenState -> LoggableScreen Action ST.MetroWarriorsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "MetroWarriorsScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ computeListItem push
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  (API.SpecialLocationListRes resp) <- fetchAndCacheStationsList initialState
                  liftFlowBT $ push $ UpdateList resp
                  response <- lift $ lift $ HelpersAPI.callApi $ API.GetSpecialLocWarriorInfoReq (getValueToLocalStore DRIVER_ID)
                  case response of
                    Right settings -> do
                      liftFlowBT $ push $ UpdateWarriorResp (makeStationsData settings resp initialState.data.remoteConfigData)
                    Left err -> do
                      let errResp = err.response
                          codeMessage = HU.decodeErrorCode errResp.errorMessage
                      void $ pure $ toast $ if err.code == 500 then getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN else codeMessage
                      pure unit
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "MetroWarriorsScreen ----- state" state
          let
            _ = spy "MetroWarriorsScreen --------action" action
          eval action state
      )
  , parent : Nothing
  , logWhitelist: initialState.data.config.logWhitelistConfig.metroWarriorsScreenLogWhitelist
  }

view :: forall w. (Action -> Effect Unit) -> ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility DISABLE
        , onBackPressed push $ const BackPressed
        , afterRender push $ const AfterRender
        , background Color.white900
        , padding $ PaddingBottom 24
        ]
        [ GenericHeader.view (push <<< GenericHeaderAction) (genericHeaderConfig state)
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 1
            , background Color.grey900
            ]
            []
        , if state.props.showShimmer then
            shimmerView state
          else if state.props.showStationList then
            metroStationListView push state
          else
            scrollView
              [ height MATCH_PARENT
              , width MATCH_PARENT
              ]
              [ linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ]
                  [ metroWarriorToggleView push state
                  , primaryMetroStationsView push state
                  ]
              ]
        ]

metroWarriorToggleView :: forall w. (Action -> Effect Unit) -> ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
metroWarriorToggleView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 16 16 16
        , orientation VERTICAL
        , cornerRadii $ Corners 6.0 true true (not isLearnMoreAvailable) (not isLearnMoreAvailable)
        , stroke $ "1," <> Color.grey700
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ height $ V 20
                , width $ V 36
                , imageWithFallback
                    $ HU.fetchImage HU.COMMON_ASSET "ny_ic_metro_dark"
                ]
            , textView
                $ [ text $ getString METRO_WARRIOR_MODE
                  ]
                <> FontStyle.body1 CTA.TypoGraphy
            , MP.layoutWithWeight
            , switchButtonView push state.data.stationData.isSpecialLocWarrior $ RefreshDataWithChanges (not state.data.stationData.isSpecialLocWarrior) state.data.stationData.primaryStation state.data.stationData.secondaryStationsData
            ]
        , textView
            $ [ text $ getString CHOOSE_METRO_STATION
              , color Color.black800
              , margin $ MarginTop 8
              ]
            <> FontStyle.body3 CTA.TypoGraphy
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 7 7
        , cornerRadii $ Corners 6.0 false false true true
        , gravity CENTER
        , background Color.blue600
        , visibility $ MP.boolToVisibility isLearnMoreAvailable
        , onClick push $ const LearnMore
        ]
        [ textView
            $ [ text $ getString LEARN_MORE
              , color Color.blue800
              ]
            <> FontStyle.tags CTA.TypoGraphy
        , imageView
            [ height $ V 22
            , width $ V 22
            , gravity RIGHT
            , margin $ MarginLeft 8
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_youtube"
            ]
        ]
    ]
  where
    isLearnMoreAvailable = not $ DS.null state.data.remoteConfigData.videoUrl

primaryMetroStationsView :: forall w. (Action -> Effect Unit) -> ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
primaryMetroStationsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , padding $ Padding 16 16 16 16
    , orientation VERTICAL
    , cornerRadius 8.0
    , stroke $ "1," <> Color.grey900
    , alpha if state.data.stationData.isSpecialLocWarrior then 1.0 else 0.5
    ]
    [ textView
        $ [ text $ getString PRIMARY_METRO_STATION
          ]
        <> FontStyle.body1 CTA.TypoGraphy
    , textView
        $ [ text $ getString PRIMARY_STATION_INFO
          , margin $ MarginTop 4
          ]
        <> FontStyle.body3 CTA.TypoGraphy
    , metroStationCard push state.data.stationData.primaryStation true state
    , textView
        $ [ text $ getString NEARBY_STATIONS
          , margin $ MarginTop 24
          , visibility $ MP.boolToVisibility $ not $ DA.null secondaryStations
          ]
        <> FontStyle.body1 CTA.TypoGraphy
    , textView
        $ [ text $ getString NEARBY_STATION_INFO
          , margin $ MarginTop 4
          , visibility $ MP.boolToVisibility $ not $ DA.null secondaryStations
          ]
        <> FontStyle.body3 CTA.TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      ( DA.mapWithIndex
          ( \index item ->
              metroStationCard push (Mb.Just item) false state
          )
          secondaryStations
      ) 
    ]
  where
  secondaryStations = getSecondaryStations state.data.stationData.primaryStation
  getSecondaryStations pStation = 
    case state.data.stationData.primaryStation of 
      Mb.Just (API.SpecialLocationWarrior pStation) -> (transformSpecialLocationToWarrior []) <$> pStation.linkedLocations
      Mb.Nothing -> []

metroStationCard :: forall w. (Action -> Effect Unit) -> Mb.Maybe API.SpecialLocationWarrior -> Boolean -> ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
metroStationCard push mbStationData isPrimary state = case mbStationData of
  Mb.Just (API.SpecialLocationWarrior card) ->
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 8
      , padding $ Padding 12 12 12 12
      , gravity CENTER_VERTICAL
      , background if isPrimary then Color.blue600 else Color.white900
      , cornerRadius 8.0
      , stroke $ "1," <> Color.grey900
      ]
      [ imageView
          [ height $ V 36
          , width $ V 48
          , background Color.grey900
          , visibility GONE -- not required
          ]
      , textView
          $ [ text card.locationName
            , margin $ MarginLeft 8
            , color Color.black800
            , weight 1.0
            ]
          <> FontStyle.tags CTA.TypoGraphy
      , imageView
          [ height $ V 12
          , margin $ MarginLeft 4
          , width $ V 12
          , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_info_black"
          , visibility GONE -- not required
          ]
      , if isPrimary then
          textView
            $ [ text $ getString CHANGE
              , color Color.blue800
              , onClick push $ const SwitchToListView
              , clickable state.data.stationData.isSpecialLocWarrior
              ]
            <> FontStyle.tags CTA.TypoGraphy
        else
          switchButtonView push (isStationEnabled card.id) 
            $ if state.data.stationData.isSpecialLocWarrior 
                then RefreshDataWithChanges true state.data.stationData.primaryStation $ updatedSecondaryStationsArray card.id state.data.stationData.secondaryStationsData
                else NoAction
      ]
  _ -> MP.noView
  where
  updatedSecondaryStationsArray id stationIds =
    if DA.elem id stationIds then
      DA.delete id stationIds
    else
      stationIds <> [ id ]
  isStationEnabled id = DA.elem id state.data.stationData.secondaryStationsData

switchButtonView :: forall w. (Action -> Effect Unit) -> Boolean -> Action -> PrestoDOM (Effect Unit) w
switchButtonView push isActive action = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , onClick push $ const action
  ]
  [ SwitchButtonView.view (push <<< SwitchButtonAction) $ SwitchButtonView.config {isActive = isActive, isClickable = false} ]
  

metroStationListView :: forall w. (Action -> Effect Unit) -> ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
metroStationListView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height $ V 50
        , orientation HORIZONTAL
        , cornerRadius 8.0
        , padding (Padding 2 2 2 2)
        , margin (Margin 16 16 16 16)
        , gravity CENTER_VERTICAL
        , stroke ("1," <> Color.borderColorLight)
        ]
        [ imageView
            [ height $ V 17
            , width $ V 17
            , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_search_grey"
            , gravity CENTER
            , margin $ Margin 10 10 10 10
            ]
        , editText $
            [ height MATCH_PARENT
            , width WRAP_CONTENT
            , color Color.black800
            , gravity LEFT
            , id (EHC.getNewIDWithTag "contactEditText")
            , background Color.white900
            , text ""
            , weight 1.0
            , hint $ getString SEARCH
            , pattern "[^\n]*,255"
            , onChange push $ StationSearchTextChanged
            ] <> FontStyle.subHeading1 CTA.TypoGraphy
        ]
    , stationListView push state
    ]

computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  listItem <- PList.preComputeListItem $ stationListCardView push
  void $ EHC.liftFlow $ push (SetListItem listItem)

stationListCardView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
stationListCardView push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingTop 8
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 16 16 16
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , gravity CENTER_VERTICAL
        , PList.onClickHolder push OnStationClick
        ]
        [ imageView
            [ height $ V 20
            , width $ V 20
            , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ic_location_unfilled"
            , margin $ MarginRight 8
            ]
        , textView
            $ [ PList.textHolder "stationName"
              , color Color.black800
              ]
            <> FontStyle.body1 CTA.TypoGraphy
        ]
    ]

stationListView :: forall w. (Action -> Effect Unit) -> ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
stationListView push state = case state.data.listItem of
  Mb.Just lItem ->
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , margin $ Margin 16 8 16 0
      ]
      [ PList.list
          [ height MATCH_PARENT
          , scrollBarY false
          , width MATCH_PARENT
          , PList.listItem lItem
          , PList.listDataV2 $ mkPlistItem <$> filteredStations
          ]
      ]
  _ -> MP.noView
  where
  mkPlistItem (API.SpecialLocation station) =
    { stationName: toPropValue station.locationName
    , id: toPropValue station.id
    }

  filteredStations = case state.data.searchString of
    Mb.Nothing -> state.data.stationList
    Mb.Just searchString -> DA.filter (\(API.SpecialLocation item) -> DS.contains (DS.Pattern (DS.toLower searchString)) (DS.toLower item.locationName)) state.data.stationList

shimmerView :: forall w. ST.MetroWarriorsScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility $ MP.boolToVisibility state.props.showShimmer
    ]
    [ sfl (V 100) 16 1 true
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginTop 24
        , padding $ Padding 16 16 16 16
        , orientation VERTICAL
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        ]
        [ sfl (V 60) 0 1 true
        , sfl (V 60) 16 1 true
        , sfl (V 60) 16 1 true
        , sfl (V 60) 16 1 true
        ]
    ]

sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop marginTop
    , visibility $ MP.boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        ( map
            ( \_ ->
                linearLayout
                  [ height height'
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , weight 1.0
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 4 4 4 4
                  ]
                  []
            )
            (1 DA... numberOfBoxes)
        )
    ]

fetchAndCacheStationsList :: ST.MetroWarriorsScreenState -> FlowBT String API.SpecialLocationListRes
fetchAndCacheStationsList state = do
  let
    storedObject = getValueToLocalStore METRO_STATIONS_LIST
    config = state.data.remoteConfigData
  case decodeCache $ parseJSON storedObject of
    Mb.Just parsedCache -> do
     if config.cacheInvalidateCounter == parsedCache.cacheInvalidateCounter && parsedCache.city == driverCity
       then pure parsedCache.list
       else callListAPI config
    Mb.Nothing -> callListAPI config
      
  where
  driverCity = getValueToLocalStore DRIVER_LOCATION
  decodeCache :: Foreign -> Mb.Maybe StationListCache
  decodeCache = hush <<< runExcept <<< decode

  callListAPI config = do
      resp <- HelpersAPI.callApiBT $ API.SpecialLocationListCategoryReq "SureMetro"
      let
        objectToCache =
          { cacheInvalidateCounter: config.cacheInvalidateCounter
          , list: resp
          , city: driverCity
          }
      void $ pure $ setValueToLocalStore METRO_STATIONS_LIST $ stringifyJSON objectToCache
      pure resp