{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Merchant
  ( module Dashboard.Common.Merchant,
    module Reexport,
  )
where

import Beckn.External.Encryption (encrypt)
import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Storage.Esqueleto (derivePersistField)
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Common
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Dashboard.Common as Reexport
import Data.Aeson
import Data.Either (isRight)
import Data.OpenApi hiding (description, name)
import Servant

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data MerchantEndpoint
  = MerchantUpdateEndpoint
  | MerchantServiceConfigUpdateEndpoint
  | MerchantServiceConfigUsageUpdateEndpoint
  deriving (Show, Read)

derivePersistField "MerchantEndpoint"

---------------------------------------------------------
-- merchant service config update -----------------------

type MerchantServiceConfigUpdateAPI =
  "serviceConfig"
    :> "maps"
    :> "update"
    :> ReqBody '[JSON] MerchantServiceConfigUpdateReq
    :> Post '[JSON] APISuccess

data MerchantServiceConfigUpdateReq
  = GoogleConfigUpdateReq GoogleCfgUpdateReq
  | OSRMConfigUpdateReq OSRMCfgUpdateReq
  | MMIConfigUpdateReq MMICfgUpdateReq
  deriving stock (Show, Generic)

data MerchantServiceConfigUpdateTReq
  = GoogleConfigUpdateTReq GoogleCfgUpdateTReq
  | OSRMConfigUpdateTReq OSRMCfgUpdateReq
  | MMIConfigUpdateTReq MMICfgUpdateTReq
  deriving stock (Generic)

instance HideSecrets MerchantServiceConfigUpdateReq where
  type ReqWithoutSecrets MerchantServiceConfigUpdateReq = MerchantServiceConfigUpdateTReq
  hideSecrets = \case
    GoogleConfigUpdateReq req -> GoogleConfigUpdateTReq $ hideSecrets req
    OSRMConfigUpdateReq req -> OSRMConfigUpdateTReq req
    MMIConfigUpdateReq req -> MMIConfigUpdateTReq $ hideSecrets req

getMapsServiceFromReq :: MerchantServiceConfigUpdateReq -> Maps.MapsService
getMapsServiceFromReq = \case
  GoogleConfigUpdateReq _ -> Maps.Google
  OSRMConfigUpdateReq _ -> Maps.OSRM
  MMIConfigUpdateReq _ -> Maps.MMI

buildMapsServiceConfig ::
  EncFlow m r =>
  MerchantServiceConfigUpdateReq ->
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

instance ToJSON MerchantServiceConfigUpdateReq where
  toJSON = genericToJSON (updateReqOptions updateReqConstructorModifier)

instance FromJSON MerchantServiceConfigUpdateReq where
  parseJSON = genericParseJSON (updateReqOptions updateReqConstructorModifier)

instance ToSchema MerchantServiceConfigUpdateReq where
  declareNamedSchema = genericDeclareNamedSchema updateReqSchemaOptions

instance ToJSON MerchantServiceConfigUpdateTReq where
  toJSON = genericToJSON (updateReqOptions updateTReqConstructorModifier)

instance FromJSON MerchantServiceConfigUpdateTReq where
  parseJSON = genericParseJSON (updateReqOptions updateTReqConstructorModifier)

updateReqOptions :: (String -> String) -> Options
updateReqOptions modifier =
  defaultOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = modifier
    }

updateReqSchemaOptions :: SchemaOptions
updateReqSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = updateReqTaggedObject,
      constructorTagModifier = updateReqConstructorModifier
    }

updateReqTaggedObject :: SumEncoding
updateReqTaggedObject =
  TaggedObject
    { tagFieldName = "serviceName",
      contentsFieldName = "serviceConfig"
    }

updateReqConstructorModifier :: String -> String
updateReqConstructorModifier = \case
  "GoogleConfigUpdateReq" -> show Maps.Google
  "OSRMConfigUpdateReq" -> show Maps.OSRM
  "MMIConfigUpdateReq" -> show Maps.MMI
  x -> x

updateTReqConstructorModifier :: String -> String
updateTReqConstructorModifier = \case
  "GoogleConfigUpdateTReq" -> show Maps.Google
  "OSRMConfigUpdateTReq" -> show Maps.OSRM
  "MMIConfigUpdateTReq" -> show Maps.MMI
  x -> x

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
  deriving stock (Show, Generic)
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
-- merchant service config usage update -----------------

type MerchantServiceConfigUsageUpdateAPI =
  "serviceUsageConfig"
    :> "maps"
    :> "update"
    :> ReqBody '[JSON] MerchantServiceUsageConfigUpdateReq
    :> Post '[JSON] APISuccess

data MerchantServiceUsageConfigUpdateReq = MerchantServiceUsageConfigUpdateReq
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

serviceUsedInReq :: MerchantServiceUsageConfigUpdateReq -> Maps.MapsService -> Bool
serviceUsedInReq (MerchantServiceUsageConfigUpdateReq a b c d e f g) service = service `elem` catMaybes [a, b, c, d, e, f, g]

instance HideSecrets MerchantServiceUsageConfigUpdateReq where
  hideSecrets = identity

validateMerchantServiceUsageConfigUpdateReq :: Validate MerchantServiceUsageConfigUpdateReq
validateMerchantServiceUsageConfigUpdateReq MerchantServiceUsageConfigUpdateReq {..} = do
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

-- TODO move to lib
data PredicateFunc a = PredicateFunc (Text -> Text) (a -> Bool)

instance Predicate a (PredicateFunc a) where
  pFun (PredicateFunc _ func) a = func a

instance ShowablePredicate (PredicateFunc a) where
  pShow (PredicateFunc mkMessage _) field = mkMessage field -- field <> " value is not allowed"

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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets FCMConfigUpdateReq where
  type ReqWithoutSecrets FCMConfigUpdateReq = FCMConfigUpdateTReq
  hideSecrets FCMConfigUpdateReq {..} = FCMConfigUpdateTReq {..}

validateFCMConfigUpdateReq :: Validate FCMConfigUpdateReq
validateFCMConfigUpdateReq FCMConfigUpdateReq {..} = do
  let mkMessage field = "Can't parse field " <> field
  validateField "fcmServiceAccount" fcmServiceAccount $ PredicateFunc mkMessage $ isRight . FCM.parseFCMAccount
