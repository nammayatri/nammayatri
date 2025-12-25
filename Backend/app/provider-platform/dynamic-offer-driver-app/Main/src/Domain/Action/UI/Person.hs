{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.Person where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Domain.Types.Person
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Types.Id
import Kernel.Utils.Common (Centesimal, maskText)

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCM.FCMRecipientToken,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    role :: Role,
    language :: Maybe Maps.Language
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> PersonAPIEntity
makePersonAPIEntity Person {..} =
  PersonAPIEntity
    { maskedMobileNumber = maskText <$> mobileNumber,
      maskedDeviceToken = FCM.FCMRecipientToken . maskText . (.getFCMRecipientToken) <$> deviceToken,
      ..
    }

getPersonNumber :: (EncFlow m r) => Person -> m (Maybe Text)
getPersonNumber person = do
  mapM decrypt person.mobileNumber

getPersonAlternateNumber :: (EncFlow m r) => Person -> m (Maybe Text)
getPersonAlternateNumber person = do
  mapM decrypt person.alternateMobileNumber

getPersonFullName :: Person -> Maybe Text
getPersonFullName person = (\fN -> fN <> maybe "" (" " <>) person.lastName) <$> Just person.firstName

roundToOneDecimal :: Centesimal -> Centesimal
roundToOneDecimal x = fromIntegral @Integer @Centesimal (round @Centesimal @Integer (x * 10)) / 10
