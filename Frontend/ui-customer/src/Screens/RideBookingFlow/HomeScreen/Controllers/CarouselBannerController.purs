module Screens.HomeScreen.Controllers.CarouselBannerController where

import Prelude
import Components.BannerCarousel as BannerCarousel
import Components.Banner as Banner
import JBridge as JB
import Data.String as STR
import Screens.Types 
import PrestoDOM 
import Screens.HomeScreen.Controllers.Types
import Language.Strings
import Language.Types
import Data.Maybe
import Screens.RideBookingFlow.HomeScreen.BannerConfig
import Data.Array
import Data.Function.Uncurried
import Engineering.Helpers.Commons
import Common.RemoteConfig.Types as CRT
import Storage
import Helpers.Utils
import Resources.Constants 
import Engineering.Helpers.Utils as EHU
import Debug
import Components.LocationTagBarV2 as LocationTagBarV2


updateBanner :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
updateBanner state = 
  if state.data.bannerData.bannerScrollState == "1" then continue state
    else do
      let nextBanner = state.data.bannerData.currentBanner + 1
          bannerArray = if state.props.currentStage == HomeScreen then getBannerConfigs state BannerCarousel else getDriverInfoCardBanners state BannerCarousel
          updatedIdx = if nextBanner >= (length bannerArray) then 0 else nextBanner
          newState = state{data {bannerData{currentBanner = updatedIdx, currentPage = updatedIdx}}}
      continue newState

bannerCarouselAC :: BannerCarousel.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
bannerCarouselAC action state = 
  case action of 
    BannerCarousel.OnClick idx -> do
      let banners = if state.props.currentStage == HomeScreen then getBannerConfigs state BannerCarousel else getDriverInfoCardBanners state BannerCarousel
      case index banners idx of
        Just config -> do
          let _ = runFn2 updatePushInIdMap "bannerCarousel" false
          case config.type of
            BannerCarousel.Gender -> continueWithCmd state [pure $ GenderBannerModal $ Banner.OnClick]
            BannerCarousel.Disability -> continueWithCmd state [pure $ DisabilityBannerAC $ Banner.OnClick]
            BannerCarousel.ZooTicket -> continueWithCmd state [pure $ TicketBookingFlowBannerAC $ Banner.OnClick]
            BannerCarousel.MetroTicket -> continueWithCmd state [pure $ MetroTicketBannerClickAC $ Banner.OnClick]
            BannerCarousel.Safety -> continueWithCmd state [pure $ SafetyBannerAction $ Banner.OnClick]
            BannerCarousel.CabLaunch -> continueWithCmd state [pure $ WhereToClick]
            BannerCarousel.Remote link ->
              if isJust config.dynamicAction then 
                handleDynamicBannerAC config.dynamicAction state 
              else if link == "search" then 
                continueWithCmd state [pure $ WhereToClick ]
              else if os == "IOS" && STR.contains (STR.Pattern "vp=sedu&option=video") link then  -- To be removed after deep links are added in iOS
                continueWithCmd state [pure GoToSafetyEducationScreen]
              else if not $ STR.null link then do
                continueWithCmd state [do 
                  void $ JB.openUrlInApp link
                  pure NoAction
                ]
              else
                update state
            _ -> update state
        Nothing -> update state
    _ -> update state

handleDynamicBannerAC :: Maybe CRT.RemoteAC ->  HomeScreenState -> Eval Action ScreenOutput HomeScreenState
handleDynamicBannerAC action state = 
  case action of 
    Just actiontype -> case actiontype of 
      CRT.Destination (CRT.DestinationParams destinationParams)-> do
        void $ pure $ updateLocalStage GoToConfirmLocation
        pure $ JB.removeMarker $ getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
        pure $ setText (getNewIDWithTag "DestinationEditText") $ fromMaybe "" destinationParams.description

        exit $ ExitAndEnterHomeScreen state {
          data{ 
            source = getString CURRENT_LOCATION
          , destination = fromMaybe "" destinationParams.description
          , destinationAddress = maybe state.data.destinationAddress (\x -> encodeAddress x [] Nothing destinationParams.lat destinationParams.lng) destinationParams.description
          }
        , props{
            destinationPlaceId = destinationParams.placeId
          , destinationLat = destinationParams.lat
          , destinationLong = destinationParams.lng
          , currentStage = GoToConfirmLocation
          , isSource = Just false
          }
        } 
        
      CRT.WhereTo -> continueWithCmd state [pure WhereToClick]
      CRT.Profile ->  exit $ GoToMyProfile state false
      CRT.UpdateProfile -> exit $ GoToMyProfile state true
      CRT.MetroBooking -> exit $ GoToMetroTicketBookingFlow state
      CRT.AmbulanceBooking -> continueWithCmd state { data { fareProductType = AMBULANCE} , props {firstTimeAmbulanceSearch = true , searchType = Just "hospital"} } [pure WhereToClick]
      CRT.ZooBooking -> exit $ GoToTicketBookingFlow state
      CRT.Safety -> exit $ GoToSafetyEducation state
      CRT.WebLink (CRT.WebLinkParams param) -> do
        continueWithCmd state [ do
          void $ JB.openUrlInApp param.url
          pure NoAction
        ]
      CRT.NoAction -> update state
      CRT.Rentals -> continueWithCmd state [pure $ LocationTagBarAC (LocationTagBarV2.TagClicked "RENTALS")]
      CRT.Intercity -> continueWithCmd state [pure $ LocationTagBarAC (LocationTagBarV2.TagClicked "INTER_CITY")]
      CRT.SafetyExplaination -> update state
      CRT.SetupSafety -> exit $ GoToNammaSafety state false false
      CRT.ReferralBanner -> exit $ GoToReferral GIVE_REFERRAL state
      CRT.IntercityBus -> continueWithCmd state [pure $ LocationTagBarAC (LocationTagBarV2.TagClicked "INTERCITY_BUS")]
    Nothing -> update state


genderBannerModal :: Banner.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
genderBannerModal action state = 
  case action of 
    Banner.OnClick -> exit $ GoToMyProfile state true
    Banner.NoAction -> update state

disabilityBannerAC :: Banner.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
disabilityBannerAC action state = 
  case action of 
    Banner.OnClick -> if (JB.addCarouselWithVideoExists unit ) then continue state{props{showEducationalCarousel = true}} else exit $ GoToMyProfile state true
    Banner.NoAction -> update state
 

ticketBookingFlowBannerAC ::Banner.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
ticketBookingFlowBannerAC action state = 
  case action of 
    Banner.OnClick -> exit $ GoToTicketBookingFlow state
    Banner.NoAction -> update state


metroTicketBannerClickAC :: Banner.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
metroTicketBannerClickAC action state = 
  case action of 
    Banner.OnClick -> exit $ GoToMetroTicketBookingFlow state
    Banner.NoAction -> update state

safetyBannerAction :: Banner.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
safetyBannerAction action state =  
  case action of 
    Banner.OnClick -> 
      if state.props.isOffline then do  
        void $ pure $ EHU.showToast (getString CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN)
        continue state
      else do 
        exit $ GoToNammaSafety state false $ state.props.sosBannerType == Just MOCK_DRILL_BANNER
    Banner.NoAction -> update state