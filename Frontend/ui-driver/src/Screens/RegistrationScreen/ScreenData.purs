{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.ScreenData where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Language.Strings (getString)
import MerchantConfig.Types (CityConfig)
import Prelude (class Eq)
import Screens.Types (RegisterationStep(..), RegistrationScreenState, StageStatus(..))
import ConfigProvider
import Foreign.Object (empty)
import Screens.Types as ST
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Array as DA
import Common.Types.App as Common
import MerchantConfig.DefaultConfig (defaultCityConfig)
import Services.API as API

initData :: RegistrationScreenState
initData = {
      data: {
        registerationStepsCabs : [],
        registerationStepsAuto : [],
        registerationStepsBike : [],
        registerationStepsAmbulance : [],
        registerationStepsTruck : [],
        registerationStepsBus : [],
        drivingLicenseStatus : NOT_STARTED,
        vehicleDetailsStatus : NOT_STARTED,
        permissionsStatus : NOT_STARTED,
        trainingsCompletionStatus : NOT_STARTED,
        vehicleTypeMismatch : false,
        documentStatusList : [],
        variantList : [],
        phoneNumber : "",
        lastUpdateTime : "",
        cityConfig : defaultCityConfig,
        config : getAppConfig appConfig,
        referralCode : "",
        referral_code_input_data : "",
        logField : empty,
        enteredDL : "",
        enteredRC : "",
        vehicleCategory : Nothing,
        linkedRc : Nothing,
        accessToken : "",
        hvTxnId : Nothing,
        hvFlowId : Nothing,
        refereeName : Nothing
      },
      props: {
        limitReachedFor : Nothing,
        logoutModalView : false,
        isValidReferralCode : true,
        enterOtpFocusIndex : 0,
        enterReferralCodeModal : false,
        referralCodeSubmitted : false,
        contactSupportView : true,
        contactSupportModal : ST.HIDE,
        selectedVehicleIndex : Nothing,
        optionalDocsExpanded : true,
        confirmChangeVehicle : false,
        refreshAnimation : false,
        driverEnabled : false,
        menuOptions : false,
        manageVehicle : false,
        manageVehicleCategory : Nothing,
        dontAllowHvRelaunch : false,
        categoryToStepProgressMap : [{category: API.PERMISSION, registrationSteps: [], completionStatus: ST.NOT_STARTED, showContinueButton: false}, {category: API.DRIVER, registrationSteps: [], completionStatus: ST.NOT_STARTED, showContinueButton: false}, {category: API.VEHICLE, registrationSteps: [], completionStatus: ST.NOT_STARTED, showContinueButton: false}, {category: API.TRAINING, registrationSteps: [], completionStatus: ST.NOT_STARTED, showContinueButton: false }],
        selectedDocumentCategory : Nothing,
        vehicleImagesUploaded : false
      }
  }
