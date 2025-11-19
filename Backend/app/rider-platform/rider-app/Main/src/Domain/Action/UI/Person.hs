{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Person where

import Data.Aeson
import qualified Data.Aeson as DA
import Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Domain.Types.SafetySettings as DSafety
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common (maskText)
import qualified Lib.Yudhishthira.Tools.Utils as YUtils

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe Text,
    hasTakenRide :: Bool,
    hasTakenValidRide :: Bool,
    referralCode :: Maybe Text,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Maps.Language,
    hasDisability :: Maybe Bool,
    disability :: Maybe Text,
    gender :: Gender,
    businessEmail :: Maybe Text,
    businessProfileVerified :: Maybe Bool,
    hasCompletedSafetySetup :: Bool,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    followsRide :: Bool,
    isSafetyCenterDisabled :: Bool,
    customerTags :: DA.Value
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> Maybe Text -> Bool -> DSafety.SafetySettings -> PersonAPIEntity
makePersonAPIEntity Person {..} disability isSafetyCenterDisabled safetySettings =
  PersonAPIEntity
    { maskedMobileNumber = maskText <$> mobileNumber,
      maskedDeviceToken = maskText <$> deviceToken,
      hasTakenRide = hasTakenValidRide,
      bundleVersion = clientBundleVersion,
      clientVersion = clientSdkVersion,
      hasCompletedMockSafetyDrill = safetySettings.hasCompletedMockSafetyDrill,
      hasCompletedSafetySetup = safetySettings.hasCompletedSafetySetup,
      customerTags = YUtils.convertTags $ fromMaybe [] customerNammaTags,
      ..
    }

data PersonCityInformation = PersonCityInformation
  { personId :: Id Person,
    currentCity :: Context.City,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, FromJSON, ToJSON)
