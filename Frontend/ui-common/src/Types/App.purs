{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Common.Types.App where

import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Generic.Rep.Eq (genericEq)
import Foreign.Generic (class Decode, class Encode)
data VehicalTypes = Sedan | Hatchback | SUV | Auto
data LazyCheck = LanguageStyle | EndPoint | BaseUrl | TypoGraphy | WithoutOffers | FunctionCall

newtype Place = Place {
  id :: String
, address :: String
, name :: String
, lat :: String
, lng :: String
}

derive instance genericPlace :: Generic Place _
derive instance newtypePlace :: Newtype Place _
instance encodePlace :: Encode Place where encode = defaultEncode
instance decodePlace :: Decode Place where decode = defaultDecode


derive instance genericVehicalTypes :: Generic VehicalTypes _
instance decodeVehicalTypes :: Decode VehicalTypes where decode = defaultEnumDecode
instance encodeVehicalTypes :: Encode VehicalTypes where encode = defaultEnumEncode
instance eqVehicalTypes :: Eq VehicalTypes where eq = genericEq


-- derive instance genericVehicalTypes :: Generic VehicalTypes
instance showVehicalTypes :: Show VehicalTypes where
    show (Sedan ) = "Sedan"
    show (Hatchback ) = "Hatchback"
    show (SUV ) = "SUV"
    show (Auto ) = "Auto"



data NotificationType = REGISTRATION_APPROVED | SEARCH_CALLBACK | CONFIRM_CALLBACK
  | TRACKING_CALLBACK | SEARCH_REQUEST | CONFIRM_REQUEST | UPCOMING_CASE

derive instance genericNotificationType :: Generic NotificationType _
instance decodeNotificationType :: Decode NotificationType where decode = defaultEnumDecode
instance encodeNotificationType :: Encode NotificationType where encode = defaultEnumEncode

newtype NotificationData = NotificationData {
  entity_type :: Maybe String
  , entity_ids :: Maybe String
  , notification_type :: Maybe String
  }

derive instance genericNotificationData :: Generic NotificationData _
derive instance newtypeNotificationData :: Newtype NotificationData _
instance encodeNotificationData :: Encode NotificationData where encode = defaultEncode
instance decodeNotificationData :: Decode NotificationData where decode = defaultDecode

newtype GlobalPayload = GlobalPayload
  { activity_recreated :: String
  , betaAssets :: Boolean
  , payload :: Payload
  , requestId :: String
  , sdkName :: String
  , sdkVersion :: String
  , service :: String
  , service_based :: Boolean
  }

derive instance newGlobalPayload :: Newtype GlobalPayload _
derive instance genericGlobalPayload :: Generic GlobalPayload _
instance decodeGlobalPayload :: Decode GlobalPayload where decode = defaultDecode
instance encodeGlobalPayload :: Encode GlobalPayload where encode = defaultEncode

newtype Payload = Payload
  { service :: String
  , environment :: String
  , notificationData :: Maybe NotificationData
  }

derive instance newPayload :: Newtype Payload _
derive instance genericPayload :: Generic Payload _
instance decodePayload :: Decode Payload where decode = defaultDecode
instance encodePayload :: Encode Payload where encode = defaultEncode

type CancellationReasons = {
    reasonCode :: String, 
    description :: String
}

-- newtype LocationLatLong = LocationLatLong
--   { lat :: String
--   , long :: String
--   }

-- derive instance genericLocationLatLong :: Generic (LocationLatLong )  _
-- derive instance newtypeLocationLatLong :: Newtype (LocationLatLong ) _
-- instance decodeLocationLatLong :: Decode (LocationLatLong ) where decode = defaultDecode
-- instance encodeLocationLatLong :: Encode (LocationLatLong ) where encode = defaultEncode

-- derive instance genericLocationLatLong :: Generic LocationLatLong _
-- derive instance newtypeLocationLatLong :: Newtype LocationLatLong _
-- instance encodeLocationLatLong :: Encode LocationLatLong where encode = defaultEncode
-- instance decodeLocationLatLong :: Decode LocationLatLong where decode = defaultDecode