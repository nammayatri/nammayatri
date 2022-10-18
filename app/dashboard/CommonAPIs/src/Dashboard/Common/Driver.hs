{-# LANGUAGE DerivingVia #-}

module Dashboard.Common.Driver where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Servant

-- there are no paths in the API types in this module because they can be different in real applications
--
data Driver

newtype DriverIds = EnableDriversRequest
  { driverIds :: [Id Driver]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---

type DriverListAPI =
  QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "verified" Bool
    :> QueryParam "enabled" Bool
    :> QueryParam "pendingdoc" Bool
    :> QueryParam "phone" Text
    :> Get '[JSON] DriverListRes

data DriverListRes = DriverListRes
  { totalItems :: Int,
    drivers :: [DriverListItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverListItem = DriverListItem
  { driverId :: Id Driver,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    vehicleNo :: Maybe Text,
    phoneNo :: Text,
    enabled :: Bool,
    verified :: Bool,
    dlStatus :: Maybe (DocumentInfo LicDetails),
    rcStatus :: Maybe (DocumentInfo RCDetails)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentInfo a = DocumentInfo
  { documentNumber :: Text,
    status :: Text,
    details :: Maybe a
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LicDetails = LicDetails
  { licExpiry :: UTCTime,
    vehicleClass :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RCDetails = RCDetails
  { vehicleClass :: Text,
    fitnessExpiry :: UTCTime,
    insuranceExpiry :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- documents info ---------------------------------------

type DriverDocumentsInfoAPI = Get '[JSON] DriverDocumentsInfoRes

data DriverDocumentsInfoRes = DriverDocumentsInfoRes
  { registered :: !Int,
    verified :: !Int,
    enabled :: !Int,
    validDocuments :: !DocumentsByStateInfo,
    invalidDocuments :: !DocumentsByStateInfo,
    verificationPending :: !DocumentsByStateInfo,
    verificationFailed :: !DocumentsByStateInfo,
    verificationLimitExceeded :: !DocumentsByStateInfo,
    docsExpiringInMonth :: !DocumentsByStateInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentsByStateInfo = DocumentsByStateInfo
  { driverLicense :: !Int,
    vehicleRegistrationCertificate :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

emptyDocumentsByStateInfo :: DocumentsByStateInfo
emptyDocumentsByStateInfo = DocumentsByStateInfo 0 0

emptyInfo :: DriverDocumentsInfoRes
emptyInfo =
  DriverDocumentsInfoRes
    { registered = 0,
      verified = 0,
      enabled = 0,
      validDocuments = emptyDocumentsByStateInfo,
      invalidDocuments = emptyDocumentsByStateInfo,
      verificationPending = emptyDocumentsByStateInfo,
      verificationFailed = emptyDocumentsByStateInfo,
      verificationLimitExceeded = emptyDocumentsByStateInfo,
      docsExpiringInMonth = emptyDocumentsByStateInfo
    }

---------------------------------------------------------
-- driver activity --------------------------------------

type DriverActivityAPI = Get '[JSON] DriverActivityRes

data DriverActivityRes = DriverActivityRes
  { activeDriversInApp :: !Int,
    --    activeDriversInLastHour :: !Int,
    inactiveDrivers :: !Int
    --    inactiveDriversSinceTwoDays :: !Int,
    --    trendFrequency :: !Seconds,
    --    trend :: ![ActivityTrendItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{-
data ActivityTrendItem = ActivityTrendItem
  { timestamp :: UTCTime,
    active :: Int,
    inactive :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
oneHour :: Seconds
oneHour = 3600
-}

emptyDriverActivityRes :: DriverActivityRes
emptyDriverActivityRes =
  DriverActivityRes
    { activeDriversInApp = 0,
      --      activeDriversInLastHour = 0,
      inactiveDrivers = 0
      --      inactiveDriversSinceTwoDays = 0,
      --      trendFrequency = oneHour,
      --      trend = []
    }

---------------------------------------------------------
-- enable drivers ---------------------------------------

type EnableDriversAPI =
  ReqBody '[JSON] DriverIds
    :> Put '[JSON] EnableDriversRes

data EnableDriversRes = EnableDriversRes
  { numDriversEnabled :: Int,
    driversEnabled :: [Id Driver],
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- disable drivers --------------------------------------

type DisableDriversAPI =
  ReqBody '[JSON] DriverIds
    :> Put '[JSON] DisableDriversRes

data DisableDriversRes = DisableDriversRes
  { numDriversDisabled :: Int,
    driversDisabled :: [Id Driver],
    message :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- driver location --------------------------------------

type DriverLocationAPI =
  QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> ReqBody '[JSON] DriverIds
    :> Get '[JSON] DriverLocationRes

data DriverLocationRes = DriverLocationRes
  { driverLocationsNotFound :: Maybe (NonEmpty (Id Driver)),
    driverLocations :: [DriverLocationItem]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationItem = DriverLocationItem
  { driverId :: Id Driver,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    vehicleNo :: Text,
    phoneNo :: Text,
    active :: Bool,
    onRide :: Bool,
    location :: LatLong,
    lastLocationTimestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
