{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.Controller where

import MerchantConfig.Types

import Components.ChatView.Controller (ChatComponentConfig)
import Components.MessagingView as MessagingView
import Components.PrimaryButton as PrimaryButtonController
import Components.SourceToDestination as SourceToDestinationController
import Data.Maybe (Maybe)
import PrestoDOM
import Screens.Types (Stage, ZoneType(..), SheetState(..), SearchResultType, City(..), BannerCarousalData(..), NavigationMode(..))
import Components.BannerCarousel as BannerCarousel
import Common.Types.App as CTP
import Services.API (Currency(..))

data Action = NoAction
            | PrimaryButtonAC PrimaryButtonController.Action
            | SourceToDestinationAC SourceToDestinationController.Action
            | CancelRide DriverInfoCardState
            | LocationTracking
            | MessageDriver
            | OnNavigate NavigationMode Number Number
            | CallDriver
            | ToggleBottomSheet
            | CollapseBottomSheet
            | ShareRide
            | RideSupport
            | StartLocationTracking String
            | WaitingInfo
            | UpdateBanner
            | BannerChanged String
            | BannerStateChanged String
            | BannerCarousel BannerCarousel.Action
            | SpecialZoneInfoTag

type DriverInfoCardState =
  { props :: DriverInfoCardProps
  , data :: DriverInfoCardData
  }

type DriverInfoCardProps =
  {
    currentStage :: Stage,
    currentSearchResultType :: SearchResultType,
    trackingEnabled :: Boolean,
    unReadMessages :: Boolean,
    showCallPopUp :: Boolean,
    isSpecialZone :: Boolean,
    estimatedTime :: String,
    zoneType :: ZoneType,
    merchantCity :: City,
    showBanner :: Boolean,
    isChatWithEMEnabled :: Boolean
  }

type DriverInfoCardData =
  { otp :: String
  , driverName :: String
  , eta :: Maybe Int
  , vehicleDetails :: String
  , registrationNumber :: String
  , rating :: Number
  , startedAt :: String
  , endedAt :: String
  , source :: String
  , destination :: String
  , rideId :: String
  , price :: Number
  , currency :: Currency
  , sourceLat :: Number
  , sourceLng :: Number
  , destinationLat :: Number
  , destinationLng :: Number
  , driverLat :: Number
  , driverLng :: Number
  , distance :: Int
  , waitingTime :: String
  , driverArrived :: Boolean
  , estimatedDistance :: String
  , driverArrivalTime :: Int
  , bppRideId :: String
  , driverNumber :: Maybe String
  , merchantExoPhone :: String
  , estimatedDropTime :: String
  , isSpecialZone :: Boolean
  , isLocationTracking :: Boolean
  , bookingCreatedAt :: String
  , config :: AppConfig
  , vehicleVariant :: String
  , defaultPeekHeight :: Int
  , bottomSheetState :: BottomSheetState
  , bannerData :: BannerCarousalData
  , bannerArray :: Array (BannerCarousel.Config (BannerCarousel.Action -> Action))
  , vehicleModel :: String
  , vehicleColor :: String
  , serviceTierName :: Maybe String
  , providerName :: String 
  , providerType :: CTP.ProviderType
  }
