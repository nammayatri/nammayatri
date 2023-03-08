{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Dashboard.Common.Merchant
  ( module Dashboard.Common.Merchant,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import Data.Either (isRight)
import Data.List.Extra (anySame)
import Data.OpenApi hiding (description, name, password, url)
import Kernel.External.Encryption (encrypt)
import qualified Kernel.External.FCM.Flow as FCM
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.SMS as SMS
import qualified Kernel.External.SMS.ExotelSms.Types as Exotel
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Predicate
import Kernel.Utils.Validation
import Servant

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data MerchantEndpoint
  = MerchantUpdateEndpoint
  | MapsServiceConfigUpdateEndpoint
  | MapsServiceConfigUsageUpdateEndpoint
  | SmsServiceConfigUpdateEndpoint
  | SmsServiceConfigUsageUpdateEndpoint
  deriving (Show, Read)

derivePersistField "MerchantEndpoint"

---------------------------------------------------------
-- merchant update --------------------------------------

data FCMConfigUpdateReq = FCMConfigUpdateReq
  { fcmUrl :: BaseUrl,
    fcmServiceAccount :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

mkFCMConfig :: Text -> FCMConfigUpdateReq -> FCM.FCMConfig
mkFCMConfig fcmTokenKeyPrefix FCMConfigUpdateReq {..} = FCM.FCMConfig {..}

newtype FCMConfigUpdateTReq = FCMConfigUpdateTReq
  { fcmUrl :: BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets FCMConfigUpdateReq where
  type ReqWithoutSecrets FCMConfigUpdateReq = FCMConfigUpdateTReq
  hideSecrets FCMConfigUpdateReq {..} = FCMConfigUpdateTReq {..}

validateFCMConfigUpdateReq :: Validate FCMConfigUpdateReq
validateFCMConfigUpdateReq FCMConfigUpdateReq {..} = do
  let mkMessage field = "Can't parse field " <> field
  validateField "fcmServiceAccount" fcmServiceAccount $ PredicateFunc mkMessage $ isRight . FCM.parseFCMAccount

---------------------------------------------------------
-- merchant maps service config update ------------------

type MapsServiceConfigUpdateAPI =
  "serviceConfig"
    :> "maps"
    :> "update"
    :> ReqBody '[JSON] MapsServiceConfigUpdateReq
    :> Post '[JSON] APISuccess

data MapsServiceConfigUpdateReq
  = GoogleConfigUpdateReq GoogleCfgUpdateReq
  | OSRMConfigUpdateReq OSRMCfgUpdateReq
  | MMIConfigUpdateReq MMICfgUpdateReq
  deriving stock (Show, Generic)

data MapsServiceConfigUpdateTReq
  = GoogleConfigUpdateTReq GoogleCfgUpdateTReq
  | OSRMConfigUpdateTReq OSRMCfgUpdateReq
  | MMIConfigUpdateTReq MMICfgUpdateTReq
  deriving stock (Generic)

instance HideSecrets MapsServiceConfigUpdateReq where
  type ReqWithoutSecrets MapsServiceConfigUpdateReq = MapsServiceConfigUpdateTReq
  hideSecrets = \case
    GoogleConfigUpdateReq req -> GoogleConfigUpdateTReq $ hideSecrets req
    OSRMConfigUpdateReq req -> OSRMConfigUpdateTReq req
    MMIConfigUpdateReq req -> MMIConfigUpdateTReq $ hideSecrets req

getMapsServiceFromReq :: MapsServiceConfigUpdateReq -> Maps.MapsService
getMapsServiceFromReq = \case
  GoogleConfigUpdateReq _ -> Maps.Google
  OSRMConfigUpdateReq _ -> Maps.OSRM
  MMIConfigUpdateReq _ -> Maps.MMI

buildMapsServiceConfig ::
  EncFlow m r =>
  MapsServiceConfigUpdateReq ->
  m Maps.MapsServiceConfig
buildMapsServiceConfig = \case
  GoogleConfigUpdateReq GoogleCfgUpdateReq {..} -> do
    googleKey' <- encrypt googleKey
    pure . Maps.GoogleConfig $ Maps.GoogleCfg {googleKey = googleKey', ..}
  OSRMConfigUpdateReq OSRMCfgUpdateReq {..} -> do
    pure . Maps.OSRMConfig $ Maps.OSRMCfg {..}
  MMIConfigUpdateReq MMICfgUpdateReq {..} -> do
    mmiAuthSecret' <- encrypt mmiAuthSecret
    mmiApiKey' <- encrypt mmiApiKey
    pure . Maps.MMIConfig $ Maps.MMICfg {mmiAuthSecret = mmiAuthSecret', mmiApiKey = mmiApiKey', ..}

instance ToJSON MapsServiceConfigUpdateReq where
  toJSON = genericToJSON (updateMapsReqOptions updateMapsReqConstructorModifier)

instance FromJSON MapsServiceConfigUpdateReq where
  parseJSON = genericParseJSON (updateMapsReqOptions updateMapsReqConstructorModifier)

instance ToSchema MapsServiceConfigUpdateReq where
  declareNamedSchema = genericDeclareNamedSchema updateMapsReqSchemaOptions

instance ToJSON MapsServiceConfigUpdateTReq where
  toJSON = genericToJSON (updateMapsReqOptions updateMapsTReqConstructorModifier)

instance FromJSON MapsServiceConfigUpdateTReq where
  parseJSON = genericParseJSON (updateMapsReqOptions updateMapsTReqConstructorModifier)

updateMapsReqOptions :: (String -> String) -> Options
updateMapsReqOptions modifier =
  defaultOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = modifier
    }

updateMapsReqSchemaOptions :: SchemaOptions
updateMapsReqSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = updateMapsReqConstructorModifier
    }

updateReqTaggedObject :: SumEncoding
updateReqTaggedObject =
  TaggedObject
    { tagFieldName = "serviceName",
      contentsFieldName = "serviceConfig"
    }

updateMapsReqConstructorModifier :: String -> String
updateMapsReqConstructorModifier = \case
  "GoogleConfigUpdateReq" -> show Maps.Google
  "OSRMConfigUpdateReq" -> show Maps.OSRM
  "MMIConfigUpdateReq" -> show Maps.MMI
  x -> x

updateMapsTReqConstructorModifier :: String -> String
updateMapsTReqConstructorModifier = \case
  "GoogleConfigUpdateTReq" -> show Maps.Google
  "OSRMConfigUpdateTReq" -> show Maps.OSRM
  "MMIConfigUpdateTReq" -> show Maps.MMI
  x -> x

-- Maps services
-- Google

data GoogleCfgUpdateReq = GoogleCfgUpdateReq
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GoogleCfgUpdateTReq = GoogleCfgUpdateTReq
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets GoogleCfgUpdateReq where
  type ReqWithoutSecrets GoogleCfgUpdateReq = GoogleCfgUpdateTReq
  hideSecrets GoogleCfgUpdateReq {..} = GoogleCfgUpdateTReq {..}

-- MMI

data MMICfgUpdateReq = MMICfgUpdateReq
  { mmiAuthUrl :: BaseUrl,
    mmiAuthId :: Text,
    mmiAuthSecret :: Text,
    mmiApiKey :: Text,
    mmiKeyUrl :: BaseUrl,
    mmiNonKeyUrl :: BaseUrl
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MMICfgUpdateTReq = MMICfgUpdateTReq
  { mmiAuthUrl :: BaseUrl,
    mmiAuthId :: Text,
    mmiKeyUrl :: BaseUrl,
    mmiNonKeyUrl :: BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets MMICfgUpdateReq where
  type ReqWithoutSecrets MMICfgUpdateReq = MMICfgUpdateTReq
  hideSecrets MMICfgUpdateReq {..} = MMICfgUpdateTReq {..}

-- OSRM

data OSRMCfgUpdateReq = OSRMCfgUpdateReq
  { osrmUrl :: BaseUrl,
    radiusDeviation :: Maybe Meters
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant sms service config update -------------------

type SmsServiceConfigUpdateAPI =
  "serviceConfig"
    :> "sms"
    :> "update"
    :> ReqBody '[JSON] SmsServiceConfigUpdateReq
    :> Post '[JSON] APISuccess

data SmsServiceConfigUpdateReq
  = MyValueFirstConfigUpdateReq MyValueFirstCfgUpdateReq
  | ExotelSmsConfigUpdateReq ExotelSmsCfgUpdateReq
  deriving stock (Show, Generic)

data SmsServiceConfigUpdateTReq
  = MyValueFirstConfigUpdateTReq MyValueFirstCfgUpdateTReq
  | ExotelSmsConfigUpdateTReq ExotelSmsCfgUpdateTReq
  deriving stock (Generic)

instance HideSecrets SmsServiceConfigUpdateReq where
  type ReqWithoutSecrets SmsServiceConfigUpdateReq = SmsServiceConfigUpdateTReq
  hideSecrets = \case
    MyValueFirstConfigUpdateReq req -> MyValueFirstConfigUpdateTReq $ hideSecrets req
    ExotelSmsConfigUpdateReq req -> ExotelSmsConfigUpdateTReq $ hideSecrets req

getSmsServiceFromReq :: SmsServiceConfigUpdateReq -> SMS.SmsService
getSmsServiceFromReq = \case
  MyValueFirstConfigUpdateReq _ -> SMS.MyValueFirst
  ExotelSmsConfigUpdateReq _ -> SMS.ExotelSms

buildSmsServiceConfig ::
  EncFlow m r =>
  SmsServiceConfigUpdateReq ->
  m SMS.SmsServiceConfig
buildSmsServiceConfig = \case
  MyValueFirstConfigUpdateReq MyValueFirstCfgUpdateReq {..} -> do
    username' <- encrypt username
    password' <- encrypt password
    pure . SMS.MyValueFirstConfig $ SMS.MyValueFirstCfg {username = username', password = password', ..}
  ExotelSmsConfigUpdateReq ExotelSmsCfgUpdateReq {..} -> do
    apiKey' <- encrypt apiKey
    apiToken' <- encrypt apiToken
    pure . SMS.ExotelSmsConfig $ SMS.ExotelSmsCfg {apiKey = apiKey', apiToken = apiToken', ..}

instance ToJSON SmsServiceConfigUpdateReq where
  toJSON = genericToJSON (updateSmsReqOptions updateSmsReqConstructorModifier)

instance FromJSON SmsServiceConfigUpdateReq where
  parseJSON = genericParseJSON (updateSmsReqOptions updateSmsReqConstructorModifier)

instance ToSchema SmsServiceConfigUpdateReq where
  declareNamedSchema = genericDeclareNamedSchema updateSmsReqSchemaOptions

instance ToJSON SmsServiceConfigUpdateTReq where
  toJSON = genericToJSON (updateSmsReqOptions updateSmsTReqConstructorModifier)

instance FromJSON SmsServiceConfigUpdateTReq where
  parseJSON = genericParseJSON (updateSmsReqOptions updateSmsTReqConstructorModifier)

updateSmsReqOptions :: (String -> String) -> Options
updateSmsReqOptions modifier =
  defaultOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = modifier
    }

updateSmsReqSchemaOptions :: SchemaOptions
updateSmsReqSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = updateSmsReqConstructorModifier
    }

updateSmsReqConstructorModifier :: String -> String
updateSmsReqConstructorModifier = \case
  "MyValueFirstConfigUpdateReq" -> show SMS.MyValueFirst
  "ExotelSmsConfigUpdateReq" -> show SMS.ExotelSms
  x -> x

updateSmsTReqConstructorModifier :: String -> String
updateSmsTReqConstructorModifier = \case
  "MyValueFirstConfigUpdateTReq" -> show SMS.MyValueFirst
  "ExotelSmsConfigUpdateTReq" -> show SMS.ExotelSms
  x -> x

-- SMS services
-- MyValueFirst

data MyValueFirstCfgUpdateReq = MyValueFirstCfgUpdateReq
  { username :: Text,
    password :: Text,
    url :: BaseUrl
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MyValueFirstCfgUpdateTReq = MyValueFirstCfgUpdateTReq
  { url :: BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets MyValueFirstCfgUpdateReq where
  type ReqWithoutSecrets MyValueFirstCfgUpdateReq = MyValueFirstCfgUpdateTReq
  hideSecrets MyValueFirstCfgUpdateReq {..} = MyValueFirstCfgUpdateTReq {..}

-- ExotelSms

data ExotelSmsCfgUpdateReq = ExotelSmsCfgUpdateReq
  { apiKey :: Text,
    apiToken :: Text,
    sid :: Exotel.ExotelSmsSID,
    url :: Exotel.ExotelURL
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExotelSmsCfgUpdateTReq = ExotelSmsCfgUpdateTReq
  { sid :: Exotel.ExotelSmsSID,
    url :: Exotel.ExotelURL
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets ExotelSmsCfgUpdateReq where
  type ReqWithoutSecrets ExotelSmsCfgUpdateReq = ExotelSmsCfgUpdateTReq
  hideSecrets ExotelSmsCfgUpdateReq {..} = ExotelSmsCfgUpdateTReq {..}

---------------------------------------------------------
-- merchant maps service config usage update ------------

type MapsServiceUsageConfigUpdateAPI =
  "serviceUsageConfig"
    :> "maps"
    :> "update"
    :> ReqBody '[JSON] MapsServiceUsageConfigUpdateReq
    :> Post '[JSON] APISuccess

data MapsServiceUsageConfigUpdateReq = MapsServiceUsageConfigUpdateReq
  { getDistances :: Maybe Maps.MapsService,
    getEstimatedPickupDistances :: Maybe Maps.MapsService,
    getDeviationDistances :: Maybe Maps.MapsService,
    getRoutes :: Maybe Maps.MapsService,
    snapToRoad :: Maybe Maps.MapsService,
    getPlaceName :: Maybe Maps.MapsService,
    getPlaceDetails :: Maybe Maps.MapsService,
    autoComplete :: Maybe Maps.MapsService
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

mapsServiceUsedInReq :: MapsServiceUsageConfigUpdateReq -> Maps.MapsService -> Bool
mapsServiceUsedInReq (MapsServiceUsageConfigUpdateReq a b c d e f g h) service = service `elem` catMaybes [a, b, c, d, e, f, g, h]

instance HideSecrets MapsServiceUsageConfigUpdateReq where
  hideSecrets = identity

validateMapsServiceUsageConfigUpdateReq :: Validate MapsServiceUsageConfigUpdateReq
validateMapsServiceUsageConfigUpdateReq MapsServiceUsageConfigUpdateReq {..} = do
  let mkMessage field = field <> " value is not allowed"
  sequenceA_
    [ validateField "getDistances" getDistances $ InMaybe $ PredicateFunc mkMessage Maps.getDistancesProvided,
      validateField "getEstimatedPickupDistances" getEstimatedPickupDistances $ InMaybe $ PredicateFunc mkMessage Maps.getDistancesProvided,
      validateField "getRoutes" getRoutes $ InMaybe $ PredicateFunc mkMessage Maps.getRoutesProvided,
      validateField "snapToRoad" snapToRoad $ InMaybe $ PredicateFunc mkMessage Maps.snapToRoadProvided,
      validateField "getPlaceName" getPlaceName $ InMaybe $ PredicateFunc mkMessage Maps.getPlaceNameProvided,
      validateField "getPlaceDetails" getPlaceDetails $ InMaybe $ PredicateFunc mkMessage Maps.getPlaceDetailsProvided,
      validateField "autoComplete" autoComplete $ InMaybe $ PredicateFunc mkMessage Maps.autoCompleteProvided,
      validateField "getDeviationDistances" getDeviationDistances $ InMaybe $ PredicateFunc mkMessage Maps.getDistancesProvided
    ]

---------------------------------------------------------
-- merchant sms service config usage update -------------

type SmsServiceUsageConfigUpdateAPI =
  "serviceUsageConfig"
    :> "sms"
    :> "update"
    :> ReqBody '[JSON] SmsServiceUsageConfigUpdateReq
    :> Post '[JSON] APISuccess

newtype SmsServiceUsageConfigUpdateReq = SmsServiceUsageConfigUpdateReq
  { smsProvidersPriorityList :: [SMS.SmsService]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

smsServiceUsedInReq :: SmsServiceUsageConfigUpdateReq -> SMS.SmsService -> Bool
smsServiceUsedInReq (SmsServiceUsageConfigUpdateReq list) service = service `elem` list

instance HideSecrets SmsServiceUsageConfigUpdateReq where
  hideSecrets = identity

validateSmsServiceUsageConfigUpdateReq :: Validate SmsServiceUsageConfigUpdateReq
validateSmsServiceUsageConfigUpdateReq SmsServiceUsageConfigUpdateReq {..} = do
  let mkMessage field = "All values in list " <> field <> " should be unique"
  validateField "smsProvidersPriorityList" smsProvidersPriorityList $ PredicateFunc mkMessage (not . anySame @SMS.SmsService)
