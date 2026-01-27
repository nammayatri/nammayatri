{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Dashboard.Common.Merchant
  ( module Dashboard.Common.Merchant,
    module Reexport,
  )
where

import Control.Applicative ((<|>))
import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight)
import Data.List.Extra (anySame)
import Data.OpenApi hiding (description, name, password, url)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Call.Types
import Kernel.External.Encryption (encrypt)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Flow as FCM
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.SMS as SMS
import qualified Kernel.External.SMS.ExotelSms.Types as Exotel
import Kernel.External.SMS.Types (SmsService (..))
import Kernel.External.Types (Language)
import qualified Kernel.External.Verification as Verification
import Kernel.External.Whatsapp.Types (WhatsappService (..))
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import qualified Kernel.Types.Id as Id
import Kernel.Types.Predicate
import qualified Kernel.Types.Registry.Subscriber as BecknSub
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Servant (FromHttpApiData (..), ToHttpApiData (..))

---------------------------------------------------------
-- merchant update --------------------------------------

data ExophoneReq = ExophoneReq
  { primaryPhone :: Text,
    backupPhone :: Text,
    callService :: CallService
  }
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateExophoneReq :: Validate ExophoneReq
validateExophoneReq ExophoneReq {..} = do
  sequenceA_
    [ validateField "primaryPhone" primaryPhone P.fullMobilePhone,
      validateField "backupPhone" backupPhone P.fullMobilePhone
    ]

data FCMConfigUpdateReq = FCMConfigUpdateReq
  { fcmUrl :: BaseUrl,
    fcmServiceAccount :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

mkFCMConfig :: Text -> FCMConfigUpdateReq -> FCM.FCMConfig
mkFCMConfig fcmTokenKeyPrefix FCMConfigUpdateReq {..} =
  FCM.FCMConfig
    { fcmUrl = fcmUrl,
      fcmServiceAccount = fcmServiceAccount,
      fcmTokenKeyPrefix = fcmTokenKeyPrefix,
      fcmNotificationObj = Nothing
    }

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
    pure . Maps.OSRMConfig $ Maps.OSRMCfg {radiusDeviation = (distanceToMeters <$> radiusDeviationWithUnit) <|> radiusDeviation, ..}
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
    googleKey :: Text,
    useAdvancedDirections :: Bool,
    googleRouteConfig :: Maps.GoogleRouteConfig,
    googlePlaceNewUrl :: BaseUrl,
    useNewPlaces :: Bool,
    googleAutocompleteParams :: Maybe [Text]
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
    mmiNonKeyUrl :: BaseUrl,
    mmiAutocompleteParams :: Maybe Text
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
    radiusDeviation :: Maybe Meters,
    radiusDeviationWithUnit :: Maybe Distance
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant sms service config update -------------------

data SmsServiceConfigUpdateReq
  = MyValueFirstConfigUpdateReq MyValueFirstCfgUpdateReq
  | ExotelSmsConfigUpdateReq ExotelSmsCfgUpdateReq
  | GupShupConfigUpdateReq GupShupCfgUpdateReq
  deriving stock (Show, Generic)

data SmsServiceConfigUpdateTReq
  = MyValueFirstConfigUpdateTReq MyValueFirstCfgUpdateTReq
  | ExotelSmsConfigUpdateTReq ExotelSmsCfgUpdateTReq
  | GupShupConfigUpdateTReq GupShupCfgUpdateTReq
  deriving stock (Generic)

instance HideSecrets SmsServiceConfigUpdateReq where
  type ReqWithoutSecrets SmsServiceConfigUpdateReq = SmsServiceConfigUpdateTReq
  hideSecrets = \case
    MyValueFirstConfigUpdateReq req -> MyValueFirstConfigUpdateTReq $ hideSecrets req
    ExotelSmsConfigUpdateReq req -> ExotelSmsConfigUpdateTReq $ hideSecrets req
    GupShupConfigUpdateReq req -> GupShupConfigUpdateTReq $ hideSecrets req

getSmsServiceFromReq :: SmsServiceConfigUpdateReq -> SMS.SmsService
getSmsServiceFromReq = \case
  MyValueFirstConfigUpdateReq _ -> SMS.MyValueFirst
  ExotelSmsConfigUpdateReq _ -> SMS.ExotelSms
  GupShupConfigUpdateReq _ -> SMS.GupShup

buildSmsServiceConfig ::
  ( EncFlow m r,
    MonadFlow m
  ) =>
  SmsServiceConfigUpdateReq ->
  m SMS.SmsServiceConfig
buildSmsServiceConfig = \case
  MyValueFirstConfigUpdateReq MyValueFirstCfgUpdateReq {..} -> do
    username' <- encrypt username
    password' <- encrypt password
    token' <- encrypt token
    pure . SMS.MyValueFirstConfig $ SMS.MyValueFirstCfg {username = username', password = password', token = token', ..}
  ExotelSmsConfigUpdateReq ExotelSmsCfgUpdateReq {..} -> do
    apiKey' <- encrypt apiKey
    apiToken' <- encrypt apiToken
    pure . SMS.ExotelSmsConfig $ SMS.ExotelSmsCfg {apiKey = apiKey', apiToken = apiToken', ..}
  GupShupConfigUpdateReq GupShupCfgUpdateReq {..} -> do
    username' <- encrypt gusername
    password' <- encrypt gpassword
    templateId' <- encrypt templateId
    entityId' <- encrypt entityId
    pure . SMS.GupShupConfig $ SMS.GupShupCfg {userName = username', password = password', entityId = entityId', templateId = templateId', ..}

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
  "GupShupConfigUpdateReq" -> show SMS.GupShup
  x -> x

updateSmsTReqConstructorModifier :: String -> String
updateSmsTReqConstructorModifier = \case
  "MyValueFirstConfigUpdateTReq" -> show SMS.MyValueFirst
  "ExotelSmsConfigUpdateTReq" -> show SMS.ExotelSms
  "GupShupConfigUpdateTReq" -> show SMS.GupShup
  x -> x

-- SMS services
-- MyValueFirst

data GupShupCfgUpdateReq = GupShupCfgUpdateReq
  { gusername :: Text,
    gpassword :: Text,
    url :: BaseUrl,
    entityId :: Text,
    templateId :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype GupShupCfgUpdateTReq = GupShupCfgUpdateTReq
  { url :: BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets GupShupCfgUpdateReq where
  type ReqWithoutSecrets GupShupCfgUpdateReq = GupShupCfgUpdateTReq
  hideSecrets GupShupCfgUpdateReq {..} = GupShupCfgUpdateTReq {..}

data MyValueFirstCfgUpdateReq = MyValueFirstCfgUpdateReq
  { username :: Text,
    password :: Text,
    url :: BaseUrl,
    token :: Text
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
-- merchant verification service config update ----------

newtype VerificationServiceConfigUpdateReq
  = IdfyConfigUpdateReq IdfyCfgUpdateReq
  deriving stock (Show, Generic)

newtype VerificationServiceConfigUpdateTReq
  = IdfyConfigUpdateTReq IdfyCfgUpdateTReq
  deriving stock (Generic)

instance HideSecrets VerificationServiceConfigUpdateReq where
  type ReqWithoutSecrets VerificationServiceConfigUpdateReq = VerificationServiceConfigUpdateTReq
  hideSecrets = \case
    IdfyConfigUpdateReq req -> IdfyConfigUpdateTReq $ hideSecrets req

getVerificationServiceFromReq :: VerificationServiceConfigUpdateReq -> Verification.VerificationService
getVerificationServiceFromReq = \case
  IdfyConfigUpdateReq _ -> Verification.Idfy

buildVerificationServiceConfig ::
  EncFlow m r =>
  VerificationServiceConfigUpdateReq ->
  m Verification.VerificationServiceConfig
buildVerificationServiceConfig = \case
  IdfyConfigUpdateReq IdfyCfgUpdateReq {..} -> do
    accountId' <- encrypt accountId
    apiKey' <- encrypt apiKey
    secret' <- encrypt secret
    pure . Verification.IdfyConfig $ Verification.IdfyCfg {accountId = accountId', apiKey = apiKey', secret = secret', ..}

instance ToJSON VerificationServiceConfigUpdateReq where
  toJSON = genericToJSON (updateVerificationReqOptions updateVerificationReqConstructorModifier)

instance FromJSON VerificationServiceConfigUpdateReq where
  parseJSON = genericParseJSON (updateVerificationReqOptions updateVerificationReqConstructorModifier)

instance ToSchema VerificationServiceConfigUpdateReq where
  declareNamedSchema = genericDeclareNamedSchema updateVerificationReqSchemaOptions

instance ToJSON VerificationServiceConfigUpdateTReq where
  toJSON = genericToJSON (updateVerificationReqOptions updateVerificationTReqConstructorModifier)

instance FromJSON VerificationServiceConfigUpdateTReq where
  parseJSON = genericParseJSON (updateVerificationReqOptions updateVerificationTReqConstructorModifier)

updateVerificationReqOptions :: (String -> String) -> Options
updateVerificationReqOptions modifier =
  defaultOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = modifier
    }

updateVerificationReqSchemaOptions :: SchemaOptions
updateVerificationReqSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = updateVerificationReqConstructorModifier
    }

updateVerificationReqConstructorModifier :: String -> String
updateVerificationReqConstructorModifier = \case
  "IdfyConfigUpdateReq" -> show Verification.Idfy
  x -> x

updateVerificationTReqConstructorModifier :: String -> String
updateVerificationTReqConstructorModifier = \case
  "IdfyConfigUpdateTReq" -> show Verification.Idfy
  x -> x

-- Verification services
-- Idfy

data IdfyCfgUpdateReq = IdfyCfgUpdateReq
  { accountId :: Text,
    apiKey :: Text,
    secret :: Text,
    url :: BaseUrl
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype IdfyCfgUpdateTReq = IdfyCfgUpdateTReq
  { url :: BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HideSecrets IdfyCfgUpdateReq where
  type ReqWithoutSecrets IdfyCfgUpdateReq = IdfyCfgUpdateTReq
  hideSecrets IdfyCfgUpdateReq {..} = IdfyCfgUpdateTReq {..}

---------------------------------------------------------
-- merchant service usage config ------------------------

-- Fields with one possible value (verificationService, initiateCall, whatsappProvidersPriorityList) not included here
data ServiceUsageConfigRes = ServiceUsageConfigRes
  { getDistances :: Maps.MapsService,
    getEstimatedPickupDistances :: Maybe Maps.MapsService,
    getRoutes :: Maps.MapsService,
    getPickupRoutes :: Maybe Maps.MapsService,
    getTripRoutes :: Maybe Maps.MapsService,
    snapToRoad :: Maps.MapsService,
    getPlaceName :: Maps.MapsService,
    getPlaceDetails :: Maps.MapsService,
    autoComplete :: Maps.MapsService,
    smsProvidersPriorityList :: [SMS.SmsService],
    snapToRoadProvidersList :: [Maps.MapsService],
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant maps service config usage update ------------

data MapsServiceUsageConfigUpdateReq = MapsServiceUsageConfigUpdateReq
  { getDistances :: Maybe Maps.MapsService,
    getEstimatedPickupDistances :: Maybe Maps.MapsService,
    getRoutes :: Maybe Maps.MapsService,
    snapToRoad :: Maybe Maps.MapsService,
    getPlaceName :: Maybe Maps.MapsService,
    getPlaceDetails :: Maybe Maps.MapsService,
    autoComplete :: Maybe Maps.MapsService
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

mapsServiceUsedInReq :: MapsServiceUsageConfigUpdateReq -> Maps.MapsService -> Bool
mapsServiceUsedInReq (MapsServiceUsageConfigUpdateReq a b c d e f g) service = service `elem` catMaybes [a, b, c, d, e, f, g]

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
      validateField "autoComplete" autoComplete $ InMaybe $ PredicateFunc mkMessage Maps.autoCompleteProvided
    ]

---------------------------------------------------------
-- merchant sms service config usage update -------------

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

---- CreateMerchantOperatingCity ------------------------

data MerchantData = MerchantData
  { name :: Text,
    shortId :: Text,
    subscriberId :: Text,
    uniqueKeyId :: Text,
    networkParticipantId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateMerchantOperatingCityReq = CreateMerchantOperatingCityReq
  { file :: FilePath,
    reqContentType :: Text,
    city :: Context.City,
    state :: Context.IndianState,
    country :: Context.Country,
    lat :: Double,
    long :: Double,
    primaryLanguage :: Maybe Language,
    supportNumber :: Maybe Text,
    cityStdCode :: Maybe Text,
    enableForMerchant :: Bool,
    exophone :: Text,
    rcNumberPrefixList :: Maybe [Text],
    currency :: Maybe Currency,
    distanceUnit :: Maybe DistanceUnit,
    merchantData :: Maybe MerchantData,
    driverOfferMerchantOperatingCityId :: Maybe Text,
    buildFRFSSubscriber :: Maybe Bool,
    baseRequestCity :: Maybe Context.City,
    baseRequestMerchant :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp CreateMerchantOperatingCityReq where
  fromMultipart form = do
    CreateMerchantOperatingCityReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)
      <*> parseInput "city" form
      <*> parseInput "state" form
      <*> parseInput "country" form
      <*> parseInput "lat" form
      <*> parseInput "long" form
      <*> parseMaybeInput "primaryLanguage" form
      <*> parseMaybeInput "supportNumber" form
      <*> parseMaybeInput "cityStdCode" form
      <*> parseInput "enableForMerchant" form
      <*> parseInput "exophone" form
      <*> parseMaybeInput "rcNumberPrefixList" form
      <*> parseMaybeInput "currency" form
      <*> parseMaybeInput "distanceUnit" form
      <*> parseMaybeJsonInput "merchantData" form
      <*> parseMaybeJsonInput "driverOfferMerchantOperatingCityId" form
      <*> parseMaybeInput "buildFRFSSubscriber" form
      <*> parseMaybeInput "baseRequestCity" form
      <*> parseMaybeInput "baseRequestMerchant" form

parseInput :: Read b => Text -> MultipartData tag -> Either String b
parseInput fieldName form = case lookupInput fieldName form of
  Right val -> maybe (Left $ "Failed to parse " ++ T.unpack fieldName ++ " input") Right (readMaybe (T.unpack val))
  Left err -> Left err

parseMaybeInput :: Read b => Text -> MultipartData tag -> Either String (Maybe b)
parseMaybeInput fieldName form = case lookupInput fieldName form of
  Right val -> Right $ readMaybe (T.unpack val)
  Left _ -> Right Nothing

parseMaybeJsonInput :: FromJSON b => Text -> MultipartData tag -> Either String (Maybe b)
parseMaybeJsonInput fieldName form = case lookupInput fieldName form of
  Right val -> do
    case eitherDecode $ BL.fromStrict (TE.encodeUtf8 val) of
      Right jsonVal -> Right $ Just jsonVal
      Left err -> Left err
  Left _ -> Right Nothing

newtype CreateMerchantOperatingCityRes = CreateMerchantOperatingCityRes
  { cityId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateMerchantOperatingCityReqT = CreateMerchantOperatingCityReqT
  { geom :: Text,
    city :: Context.City,
    state :: Context.IndianState,
    country :: Context.Country,
    lat :: Double,
    long :: Double,
    primaryLanguage :: Maybe Language,
    supportNumber :: Maybe Text,
    cityStdCode :: Maybe Text,
    enableForMerchant :: Bool,
    exophone :: Text,
    rcNumberPrefixList :: Maybe [Text],
    currency :: Maybe Currency,
    distanceUnit :: Maybe DistanceUnit,
    merchantData :: Maybe MerchantData,
    driverOfferMerchantOperatingCityId :: Maybe Text,
    buildFRFSSubscriber :: Maybe Bool,
    baseRequestCity :: Maybe Context.City,
    baseRequestMerchant :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateMerchantOperatingCityReq where
  hideSecrets = identity

---- UpsertSpecialLocation ---------------------------------------

data UpsertSpecialLocationReq = UpsertSpecialLocationReq
  { file :: Maybe FilePath,
    reqContentType :: Maybe Text,
    locationName :: Maybe Text,
    category :: Maybe Text,
    city :: Maybe Context.City
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UpsertSpecialLocationReq where
  fromMultipart form = do
    let eitherFile = lookupFile "file" form
        (fileField, contentType_) = getFileAndFileType eitherFile
    ( UpsertSpecialLocationReq fileField contentType_
        <$> parseMaybeInput "locationName" form
      )
      <*> parseMaybeInput "category" form
      <*> parseMaybeInput "city" form

instance HideSecrets UpsertSpecialLocationReq where
  hideSecrets = identity

getFileAndFileType :: Either String (FileData tag) -> (Maybe (MultipartResult tag), Maybe Text)
getFileAndFileType ethFile =
  case ethFile of
    Right fileData -> do
      let payload = fdPayload fileData
          fileCType = fdFileCType fileData
      (Just payload, Just fileCType)
    Left _err -> (Nothing, Nothing)

data UpsertSpecialLocationReqT = UpsertSpecialLocationReqT
  { locationName :: Maybe Text,
    geom :: Maybe Text,
    category :: Maybe Text,
    city :: Maybe Context.City
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

------- UpsertSpecialLocationGates ---------------------------------------

data UpsertSpecialLocationGateReq = UpsertSpecialLocationGateReq
  { file :: Maybe FilePath,
    reqContentType :: Maybe Text,
    name :: Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double,
    defaultDriverExtra :: Maybe Int,
    address :: Maybe Text,
    canQueueUpOnGate :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UpsertSpecialLocationGateReq where
  fromMultipart form = do
    let eitherFile = lookupFile "file" form
        (fileField, contentType_) = getFileAndFileType eitherFile
    ( UpsertSpecialLocationGateReq fileField contentType_
        <$> parseInput "name" form
      )
      <*> parseMaybeInput "latitude" form
      <*> parseMaybeInput "longitude" form
      <*> parseMaybeInput "defaultDriverExtra" form
      <*> parseMaybeInput "address" form
      <*> parseMaybeInput "canQueueUpOnGate" form

instance HideSecrets UpsertSpecialLocationGateReq where
  hideSecrets = identity

data UpsertSpecialLocationGateReqT = UpsertSpecialLocationGateReqT
  { name :: Text,
    geom :: Maybe Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double,
    defaultDriverExtra :: Maybe Int,
    address :: Maybe Text,
    canQueueUpOnGate :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---- DeleteSpecialLocationGate ----------------------------------

data SupportedVehicleClasses = DLValidClasses [Text] | RCValidClasses [VehicleClassVariantMap]
  deriving stock (Generic, Show)

instance ToJSON SupportedVehicleClasses where
  toJSON = genericToJSON supportedVCOptions

instance FromJSON SupportedVehicleClasses where
  parseJSON = genericParseJSON supportedVCOptions

instance ToSchema SupportedVehicleClasses where
  declareNamedSchema = genericDeclareNamedSchema supportedVCSchemaOptions

supportedVCOptions :: Options
supportedVCOptions =
  defaultOptions
    { sumEncoding = supportedVCTaggedObject,
      constructorTagModifier = supportedVCModifier
    }

supportedVCSchemaOptions :: SchemaOptions
supportedVCSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = supportedVCTaggedObject,
      constructorTagModifier = supportedVCModifier
    }

supportedVCTaggedObject :: SumEncoding
supportedVCTaggedObject =
  TaggedObject
    { tagFieldName = "documentType",
      contentsFieldName = "vehicleClasses"
    }

supportedVCModifier :: String -> String
supportedVCModifier = \case
  "DL" -> "DLValidClasses"
  "RC" -> "RCValidClasses"
  x -> x

data VehicleClassVariantMap = VehicleClassVariantMap
  { vehicleClass :: Text,
    vehicleCapacity :: Maybe Int,
    vehicleVariant :: VehicleVariant,
    manufacturer :: Maybe Text,
    manufacturerModel :: Maybe Text,
    reviewRequired :: Maybe Bool,
    vehicleModel :: Text,
    bodyType :: Maybe Text,
    priority :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ConfigFailoverReq = ConfigFailoverReq
  { merchantOperatingCity :: Kernel.Prelude.Maybe Context.City,
    priorityOrder :: Kernel.Prelude.Maybe PriorityListWrapperType
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ConfigFailoverReq where
  hideSecrets = Kernel.Prelude.identity

data ConfigNames
  = BecknNetwork
  | SmsProvider
  | WhatsappProvider
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

instance FromHttpApiData ConfigNames where
  parseQueryParam = readEither

instance ToHttpApiData ConfigNames where
  toUrlPiece = show

data NetworkEnums
  = ONDC
  | NY
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

instance FromHttpApiData NetworkEnums where
  parseQueryParam = readEither

instance ToHttpApiData NetworkEnums where
  toUrlPiece = show

data PriorityListWrapperType = PriorityListWrapperType
  { networkTypes :: [NetworkEnums],
    smsProviders :: [SmsService],
    whatsappProviders :: [WhatsappService]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--- Upsert special location using csv file ----

data UpsertSpecialLocationCsvReq = UpsertSpecialLocationCsvReq {locationGeoms :: [(Text, Kernel.Prelude.FilePath)], gateGeoms :: [(Text, Kernel.Prelude.FilePath)], file :: Kernel.Prelude.FilePath, upsertInDriverApp :: Maybe Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpsertSpecialLocationCsvReq where
  hideSecrets = identity

instance FromMultipart Tmp UpsertSpecialLocationCsvReq where
  fromMultipart form = do
    let locationGeoms = map (\file -> (fdFileName file, fdPayload file)) (filter (\file -> fdInputName file == T.pack "locationGeoms") $ files form)
        gateGeoms = map (\file -> (fdFileName file, fdPayload file)) (filter (\file -> fdInputName file == T.pack "gateGeoms") $ files form)
    csvFile <- fmap fdPayload (lookupFile "file" form)
    let upsertInDriverApp = either (const Nothing) (readMaybe . T.unpack) $ lookupInput "upsertInDriverApp" form
    return $ UpsertSpecialLocationCsvReq locationGeoms gateGeoms csvFile upsertInDriverApp

instance ToMultipart Tmp UpsertSpecialLocationCsvReq where
  toMultipart form =
    MultipartData
      (maybe [] (\val -> [Input "upsertInDriverApp" (T.pack $ show val)]) form.upsertInDriverApp)
      ( [FileData "file" (T.pack form.file) "" (form.file)]
          <> (map (\(fileName, file) -> FileData "locationGeoms" fileName (T.pack file) file) form.locationGeoms)
          <> (map (\(fileName, file) -> FileData "gateGeoms" fileName (T.pack file) file) form.gateGeoms)
      )

newtype APISuccessWithUnprocessedEntities = APISuccessWithUnprocessedEntities {unprocessedEntities :: [Kernel.Prelude.Text]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--- Upsert driver pool config using csv file ----

newtype UpsertDriverPoolConfigCsvReq = UpsertDriverPoolConfigCsvReq {file :: Kernel.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpsertDriverPoolConfigCsvReq where
  hideSecrets = identity

instance FromMultipart Tmp UpsertDriverPoolConfigCsvReq where
  fromMultipart form = do
    UpsertDriverPoolConfigCsvReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpsertDriverPoolConfigCsvReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

data UpdateGeometryReq = UpdateGeometryReq {region :: Text, file :: Kernel.Prelude.FilePath, updateInDriver :: Maybe Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateGeometryReq where
  hideSecrets = identity

instance FromMultipart Tmp UpdateGeometryReq where
  fromMultipart form = do
    region <- lookupInput "region" form
    csvFile <- fmap fdPayload (lookupFile "file" form)
    let updateInDriver = either (const Nothing) (readMaybe . T.unpack) $ lookupInput "updateInDriver" form
    return $ UpdateGeometryReq region csvFile updateInDriver

instance ToMultipart Tmp UpdateGeometryReq where
  toMultipart form =
    MultipartData
      ( [Input "region" form.region]
          <> maybe [] (\val -> [Input "updateInDriver" (T.pack $ show val)]) form.updateInDriver
      )
      [FileData "file" (T.pack form.file) "" (form.file)]

------------------------ WhiteList Operating City ------------------------

data WhiteListOperatingCityReq = WhiteListOperatingCityReq
  { bppMerchantOperatingCityId :: Text,
    bppMerchantId :: Text,
    bppSubscriberDomain :: Context.Domain,
    bppSubscriberId :: Id.ShortId BecknSub.Subscriber,
    bapMerchantId :: Text,
    bapMerchantOperatingCityId :: Text,
    bapUniqueKeyId :: Text,
    bapSubscriberId :: Id.ShortId BecknSub.Subscriber,
    bapSubscriberDomain :: Context.Domain
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WhiteListOperatingCityRes = WhiteListOperatingCityRes
  { whiteListSuccess :: Bool,
    whiteListMessage :: Text,
    whiteListError :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets WhiteListOperatingCityReq where
  hideSecrets = identity
