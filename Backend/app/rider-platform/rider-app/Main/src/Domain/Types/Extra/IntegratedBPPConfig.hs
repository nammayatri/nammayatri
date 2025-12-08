{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.IntegratedBPPConfig where

import Data.Aeson
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
    qrRefreshTtl :: Maybe Seconds
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
    redisPrefix :: Maybe Text
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
    enableBookType :: Maybe Bool
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
    bookingEndTime :: UTCTime,
    bookingStartTime :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, Eq)
