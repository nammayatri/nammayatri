{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Extra.IntegratedBPPConfig where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Base64
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Time
import qualified Text.Show (show)

data CRISRouteSortingCriteria = FARE | DISTANCE
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq)

data MergeQuoteCriteria = FULFILLMENT | QUOTE_TYPE
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq)

data QREncoding = LATIN1
  deriving (Generic, Show, Read, FromJSON, ToJSON, Eq, ToSchema)

data EBIXConfig = EBIXConfig
  { agentId :: Text,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    networkHostUrl :: BaseUrl
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show EBIXConfig where
  show _ = "EBIXConfig"

data DIRECTConfig = DIRECTConfig
  { cipherKey :: Base64,
    qrRefreshTtl :: Maybe Seconds,
    redisPrefix :: Maybe Text,
    busBlockExpiryTime :: Maybe Seconds,
    busBlockMaxLimit :: Maybe Int
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show DIRECTConfig where
  show _ = "DIRECTConfig"

data CMRLConfig = CMRLConfig
  { networkHostUrl :: BaseUrl,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show CMRLConfig where
  show _ = "CMRLConfig"

data CMRLV2Config = CMRLV2Config
  { networkHostUrl :: BaseUrl,
    username :: Text,
    password :: EncryptedField 'AsEncrypted Text,
    operatorNameId :: Int,
    merchantId :: Text,
    ticketTypeId :: Int,
    ticketTypeIds :: Maybe (HM.HashMap Text Int),
    fareTypeId :: Int,
    encKeyIndex :: Int,
    encryptionKey :: EncryptedField 'AsEncrypted Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show CMRLV2Config where
  show _ = "CMRLV2Config"

data KMRLConfig = KMRLConfig
  { tokenUrl :: BaseUrl,
    fareUrl :: BaseUrl,
    bookTicketUrl :: BaseUrl,
    ticketStatusUrl :: BaseUrl,
    softCancelUrl :: BaseUrl,
    hardCancelUrl :: BaseUrl,
    stationListUrl :: BaseUrl,
    ibmClientId :: Text,
    ibmClientSecret :: EncryptedField 'AsEncrypted Text,
    fapiChannelId :: Text,
    kmrlAuthUserId :: Text,
    kmrlAuthPassword :: EncryptedField 'AsEncrypted Text,
    kmrlChannelId :: Text,
    clientCertPem :: EncryptedField 'AsEncrypted Text,
    operatorPublicCertPem :: Text,
    signingPrivateKeyPem :: EncryptedField 'AsEncrypted Text,
    serverCaPem :: Maybe Text,
    -- | @kochi_bap_orderid_prefix@: ONDC @order.id@ prefix keyed by the lowercased buyer
    -- domain. KMRL reconciles settlement against @order.id@, so each buyer app needs its
    -- own prefix here; an unmapped buyer keeps the default.
    bapOrderIdPrefixes :: Maybe (HM.HashMap Text Text),
    bapTransactionIdPrefixes :: Maybe (HM.HashMap Text Text),
    defaultTransactionIdPrefix :: Text,
    metroType :: Int
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show KMRLConfig where
  show _ = "KMRLConfig"

data ONDCBecknConfig = ONDCBecknConfig
  { networkHostUrl :: Maybe BaseUrl,
    networkId :: Maybe Text,
    multiInitAllowed :: Maybe Bool,
    fareCachingAllowed :: Maybe Bool,
    singleTicketForMultiplePassengers :: Maybe Bool,
    mergeQuoteCriteria :: Maybe MergeQuoteCriteria,
    routeBasedQuoteSelection :: Maybe Bool,
    providerInfo :: Maybe ProviderLevelInfo,
    routeBasedVehicleTracking :: Maybe Bool,
    overrideCity :: Maybe Context.City,
    redisPrefix :: Maybe Text,
    busBlockExpiryTime :: Maybe Seconds,
    busBlockMaxLimit :: Maybe Int,
    qrEncoding :: Maybe QREncoding
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show ONDCBecknConfig where
  show _ = "ONDCBecknConfig"

data CRISConfig = CRISConfig
  { baseUrl :: BaseUrl,
    consumerKey :: EncryptedField 'AsEncrypted Text,
    consumerSecret :: EncryptedField 'AsEncrypted Text,
    decryptionKey :: EncryptedField 'AsEncrypted Text,
    clientSecret :: EncryptedField 'AsEncrypted Text,
    encryptionKey :: EncryptedField 'AsEncrypted Text,
    appCode :: Text,
    tpAccountId :: Int,
    sourceZone :: Text,
    ticketType :: Text,
    changeOverIndirectStations :: Maybe [Text],
    changeOverDirectStations :: Maybe [Text],
    agentDataDecryptionKey :: EncryptedField 'AsEncrypted Text,
    utsDataKey :: EncryptedField 'AsEncrypted Text,
    routeSortingCriteria :: Maybe CRISRouteSortingCriteria,
    reconDuration :: Maybe Int,
    singleModeWalkThreshold :: Maybe Int,
    useRouteFareV4 :: Maybe Bool,
    enableBookType :: Maybe Bool,
    balanceCheckTimeOfDay :: Maybe Int, -- Time in seconds from midnight (e.g., 86340 for 11:59 PM)
    corridorStations :: Maybe [Text],
    enableCorridorDeprioritization :: Maybe Bool
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Show CRISConfig where
  show _ = "CRISConfig"

data ProviderLevelInfo = ProviderLevelInfo
  { providerId :: Text,
    providerName :: Maybe Text,
    oneWayTicketLimit :: Int,
    roundTripTicketLimit :: Int,
    isCancellationAllowed :: Bool,
    isTechnicalCancellationAllowed :: Maybe Bool,
    bookingEndTime :: UTCTime,
    bookingStartTime :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, Eq)

data OperatorCancellation = OperatorCancellation
  { termsUrl :: Text,
    isAllowed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OperatorRecon = OperatorRecon
  { domain :: Text,
    coreVersion :: Text,
    ttl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OperatorCatalog = OperatorCatalog
  { brandName :: Text,
    brandLogoUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OperatorQuoteCache = OperatorQuoteCache
  { ttlSeconds :: Int,
    heldTtlSeconds :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OperatorConfig = OperatorConfig
  { businessTermsUrl :: Text,
    courtJurisdiction :: Text,
    cancellation :: OperatorCancellation,
    maxPaidAreaMinutes :: Maybe Int,
    oneWayTicketLimit :: Int,
    roundTripTicketLimit :: Int,
    ticketValidityLabel :: Text,
    ticketValidityDuration :: Text,
    recon :: OperatorRecon,
    catalog :: OperatorCatalog,
    quoteCache :: OperatorQuoteCache,
    defaultOrderIdPrefix :: Text,
    sellerEntityInfo :: Maybe Value,
    serviceableBapIds :: Maybe (HM.HashMap Text [Text])
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
