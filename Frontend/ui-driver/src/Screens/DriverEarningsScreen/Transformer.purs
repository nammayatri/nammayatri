{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreen.Transformer where

import Prelude
import Data.Maybe
import Screens.Types
import Services.API as API
import Storage
import Language.Strings (getString)
import Language.Types (STR(..))
import Locale.Utils

getEventName :: DriverEarningsScreenState -> API.DriverCoinsFunctionType -> Maybe API.BulkCoinTitleTranslations -> String
getEventName state event bulkUploadTitle = case event of
  API.OneOrTwoStarRating -> getString BAD_RATING_BY_CUSTOMER
  API.RideCompleted -> getString RIDE_COMPLETED
  API.FiveStarRating -> getString GOOD_RATING_BY_CUSTOMER
  API.BookingCancellation -> getString RIDE_CANCELLATION
  API.CustomerReferral -> getString CUSTOMER_REFERRAL
  API.DriverReferral -> getString DRIVER_REFERRAL
  API.EightPlusRidesInOneDay -> state.data.config.coinsConfig.numOfRideThresholdForCoins <> getString RIDES_IN_A_DAY
  API.PurpleRideCompleted -> getString PURPLE_RIDE_COMPLETED
  API.LeaderBoardTopFiveHundred -> getString TOP <> " " <> state.data.config.coinsConfig.leaderBoardThresholdForCoins <> " " <> getString IN_WEEKLY_LEADERBOARD
  API.TrainingCompleted -> getString TRAINING_COMPLTED
  API.BulkUploadFunction -> case bulkUploadTitle of
    Just (API.BulkCoinTitleTranslations title) -> case getLanguageLocale languageKey of
                       "HI_IN" -> title.hi
                       "KN_IN" -> title.kn
                       "TA_IN" -> title.ta
                       "TE_IN" -> title.te
                       "FR_FR" -> title.fr
                       "ML_IN" -> title.ml
                       "BN_IN" -> title.bn
                       _       -> title.en
    Nothing -> getString BONUS_COINS
