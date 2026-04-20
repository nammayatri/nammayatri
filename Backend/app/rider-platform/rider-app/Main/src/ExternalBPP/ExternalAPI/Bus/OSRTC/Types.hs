module ExternalBPP.ExternalAPI.Bus.OSRTC.Types where

import Control.Applicative ((<|>))
import Data.Aeson (Value, defaultOptions, genericToEncoding, object, withObject, (.:))
import Data.Aeson.Types (Options, fieldLabelModifier, withText)
import qualified Data.Text as T
import Data.Time (Day)
import qualified ExternalBPP.ExternalAPI.Bus.OSRTC.Enums as OSRTCEnums
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

-- Response wrapper for all OSRTC responses
data OSRTCResponse a = OSRTCResponse
  { _data :: Maybe a, -- OSRTC sends null on failure; check returnCode/returnMessage before unwrapping
    returnCode :: Int,
    returnMessage :: Text,
    isSuccess :: Bool,
    exception :: Maybe Text,
    rowcount :: Int
  }
  deriving (Generic, Show)

instance (FromJSON a) => FromJSON (OSRTCResponse a) where
  parseJSON = genericParseJSON osrtcResponseOptions

instance (ToJSON a) => ToJSON (OSRTCResponse a) where
  toJSON = genericToJSON osrtcResponseOptions
  toEncoding = genericToEncoding osrtcResponseOptions

osrtcResponseOptions :: Options
osrtcResponseOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "_data" -> "data"
        "isSuccess" -> "issuccess"
        s -> s
    }

-- GenerateAccess
data OSRTCGenerateAccessReq = OSRTCGenerateAccessReq
  { userName :: Text,
    secretKey :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance Show OSRTCGenerateAccessReq where
  showsPrec _ r = showString $ "OSRTCGenerateAccessReq {userName = " <> T.unpack r.userName <> ", secretKey = <redacted>}"

type OSRTCGenerateAccessResList = [OSRTCGenerateAccessRes]

data OSRTCGenerateAccessRes = OSRTCGenerateAccessRes
  { strUserName :: Text,
    strAccessToken :: Text,
    dteAccessTokenExpirationTime :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

instance Show OSRTCGenerateAccessRes where
  showsPrec _ r = showString $ "OSRTCGenerateAccessRes {strUserName = " <> T.unpack r.strUserName <> ", strAccessToken = <redacted>}"

-- GetStationList
type OSRTCGetStationListRes = [OSRTCStation]

data OSRTCGetStationListReq = OSRTCGetStationListReq
  { intPlatformID :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OSRTCStation = OSRTCStation
  { intStationID :: Int,
    strStationName :: Text,
    strStationCode :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- SearchTrip
data OSRTCSearchTripReq = OSRTCSearchTripReq
  { intFromStationID :: Int,
    intToStationID :: Int,
    dteJourneyDate :: Day,
    intPlatformID :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OSRTCSearchTripRes = [OSRTCTripObject]

data OSRTCFareRow = OSRTCFareRow
  { intTotalKM :: Double,
    decFareAmount :: HighPrecMoney,
    intSeatTypeID :: OSRTCEnums.OSRTCSeatType,
    intTicketCategoryID :: OSRTCEnums.OSRTCTicketCategory
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OSRTCPickupPoint = OSRTCPickupPoint
  { pickUpPoint :: Text,
    stationName :: Text,
    tmArrivalTime :: Text,
    tmDepatureTime :: Text,
    pickUpPointType :: OSRTCPickupPointType,
    intPickUpPointID :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON OSRTCPickupPoint where
  parseJSON = genericParseJSON osrtcPickupPointJSONOptions

instance ToJSON OSRTCPickupPoint where
  toJSON = genericToJSON osrtcPickupPointJSONOptions
  toEncoding = genericToEncoding osrtcPickupPointJSONOptions

osrtcPickupPointJSONOptions :: Options
osrtcPickupPointJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "pickUpPoint" -> "PickUpPoint"
        "stationName" -> "StationName"
        "pickUpPointType" -> "PickUpPointType"
        s -> s
    }

data OSRTCPickupPointType = BoardingPoint | DroppingPoint
  deriving (Generic, Show)

instance FromJSON OSRTCPickupPointType where
  parseJSON = withText "OSRTCPickupPointType" $ \case
    "Boarding Point" -> return BoardingPoint
    "Dropping Point" -> return DroppingPoint
    t -> fail $ "OSRTCPickupPointType.fromJSON: unexpected value " <> show t

instance ToJSON OSRTCPickupPointType where
  toJSON BoardingPoint = "Boarding Point"
  toJSON DroppingPoint = "Dropping Point"

-- | Search-trip row; JSON keys follow OSRTC (mixed PascalCase and @str*\/int*\/dte*@ prefixes).
data OSRTCTripObject = OSRTCTripObject
  { fare :: [OSRTCFareRow],
    strOrigin :: Text, -- not necessarily same as strFromStation
    totalSeats :: Int,
    decMinFare :: HighPrecMoney,
    intTotalKM :: Int,
    strBusType :: Text,
    strRouteID :: Text,
    vacentSeats :: Int,
    dteTripDate :: Day,
    pickupPoints :: [OSRTCPickupPoint],
    intBusTypeID :: Int,
    intVehicleID :: Maybe Int,
    strToStation :: Text,
    strVehicleNo :: Text,
    occupiedSeats :: Int,
    tripDirection :: Text,
    bAllowBooking :: Bool,
    strTripStatus :: Text,
    tmArrivalTime :: Text,
    intToStationID :: Int,
    strDestination :: Text,
    strFromStation :: Text,
    tmDepatureTime :: Text,
    dteTravelEndDate :: Text,
    intCorporationID :: Int,
    intFromStationID :: Int,
    intServiceTripID :: Int,
    intRefundMatrixID :: Int,
    dteTravelStartDate :: Text,
    strServiceTripCode :: Text,
    strServiceTypeName :: Text,
    singleAvailableSeat :: Int,
    intMinimumKilometer :: Int,
    strTotalJourneyTime :: Text,
    intSeatLayoutGroupID :: Int,
    intServiceTypeMasterID :: Int,
    strServiceOperatorName :: Text, -- should be OSRTC
    intServiceTripDepartureID :: Int,
    strOrganizationEntityName :: Text -- should be OSRTC
  }
  deriving stock (Generic, Show)

instance FromJSON OSRTCTripObject where
  parseJSON = genericParseJSON osrtcTripObjectJSONOptions

instance ToJSON OSRTCTripObject where
  toJSON = genericToJSON osrtcTripObjectJSONOptions
  toEncoding = genericToEncoding osrtcTripObjectJSONOptions

osrtcTripObjectJSONOptions :: Options
osrtcTripObjectJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "fare" -> "Fare"
        "totalSeats" -> "TotalSeats"
        "vacentSeats" -> "VacentSeats"
        "dteTripDate" -> "dtetripdate"
        "pickupPoints" -> "PickupPoints"
        "occupiedSeats" -> "OccupiedSeats"
        "tripDirection" -> "TripDirection"
        "singleAvailableSeat" -> "SingleAvailableSeat"
        s -> s
    }

-- GetSeatAvailability
data OSRTCSeatAvailabilityReq = OSRTCSeatAvailabilityReq
  { intServiceTripDepartureID :: Int,
    intFromStationID :: Int,
    intToStationID :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OSRTCSeatAvailabilityRes = OSRTCSeatAvailabilityRes
  { intTotalCount :: Int,
    intAvailableCount :: Int,
    intUnAvailableCount :: Int,
    intSingleAvailableSeats :: Int,
    intServiceTripDepartureID :: Int,
    lstSeats :: [OSRTCSeatInfo]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OSRTCSeatInfo = OSRTCSeatInfo
  { col :: Int,
    row :: Int,
    zindex :: Int,
    seatLength :: Int,
    seatWidth :: Int,
    seatSrNo :: Text,
    seatCode :: Text,
    quotaCode :: Maybe OSRTCEnums.OSRTCQuota, -- Nothing, for blocked-space rows (0)
    seatStatusCode :: Text,
    strColorCode :: Text,
    isAvailable :: Bool,
    layoutComponentID :: Int,
    componentName :: Text,
    fare :: Text,
    minFare :: Text,
    netFare :: Text,
    seatTypeID :: Maybe OSRTCEnums.OSRTCSeatType, -- Nothing, for blocked-space rows (NA)
    berthTypeID :: OSRTCEnums.OSRTCBerthType,
    intFareManagementID :: Int,
    selected :: Bool,
    booked :: Bool,
    available :: Bool,
    reserved :: Bool,
    lady :: Bool,
    sleeper :: Bool,
    disabled :: Bool,
    seatObjType :: Text,
    quota :: Text,
    blocked :: Bool,
    intQuotaID :: Int
  }
  deriving stock (Generic, Show)

-- Manual decoder so quotaCode / seatTypeID can fall back to Nothing on the
-- "NA" / 0 sentinels Amnex emits for non-passenger layout rows (Blocked Space
-- etc.). genericParseJSON would propagate the inner enum-parse failure for the
-- whole list and break the entire seatAvailability response.
instance FromJSON OSRTCSeatInfo where
  parseJSON = withObject "OSRTCSeatInfo" $ \o -> do
    OSRTCSeatInfo
      <$> o .: "col"
      <*> o .: "row"
      <*> o .: "zindex"
      <*> o .: "length"
      <*> o .: "width"
      <*> o .: "seatSrNo"
      <*> o .: "seatCode"
      <*> ((Just <$> o .: "quotaCode") <|> pure Nothing)
      <*> o .: "seatStatusCode"
      <*> o .: "strColorCode"
      <*> o .: "isAvailable"
      <*> o .: "layoutComponentID"
      <*> o .: "componentName"
      <*> o .: "fare"
      <*> o .: "minFare"
      <*> o .: "netFare"
      <*> ((Just <$> o .: "seatTypeID") <|> pure Nothing)
      <*> o .: "berthTypeID"
      <*> o .: "intFareManagementID"
      <*> o .: "selected"
      <*> o .: "booked"
      <*> o .: "available"
      <*> o .: "reserved"
      <*> o .: "lady"
      <*> o .: "sleeper"
      <*> o .: "disabled"
      <*> o .: "type"
      <*> o .: "quota"
      <*> o .: "blocked"
      <*> o .: "intQuotaID"

instance ToJSON OSRTCSeatInfo where
  toJSON = genericToJSON osrtcSeatInfoJSONOptions
  toEncoding = genericToEncoding osrtcSeatInfoJSONOptions

osrtcSeatInfoJSONOptions :: Options
osrtcSeatInfoJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "seatLength" -> "length"
        "seatWidth" -> "width"
        "seatObjType" -> "type"
        s -> s
    }

-- TicketFareCalculation
data OSRTCCategoryWiseFareReq = OSRTCCategoryWiseFareReq
  { intSrNo :: Int,
    strSelectedSeatCode :: Text,
    strSelectedSeatNo :: Text,
    intSeatTypeID :: OSRTCEnums.OSRTCSeatType,
    intTicketCategoryID :: OSRTCEnums.OSRTCTicketCategory
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OSRTCFareCalcReq = OSRTCFareCalcReq
  { intServiceTripDepartureID :: Int,
    intFromStationID :: Int,
    intToStationID :: Int,
    intPlatformID :: Int,
    strCategoryWiseJSON :: [OSRTCCategoryWiseFareReq]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OSRTCFareCalcResList = [OSRTCFareCalcRes]

data OSRTCFareCalcRes = OSRTCFareCalcRes
  { lstFinalFare :: [OSRTCFareComponent],
    lstSeatWiseFare :: [Value],
    lstStateWiseSeatWiseFare :: Maybe [Value],
    intServiceTripID :: Int,
    intTotalBaseFare :: Double,
    -- OSRTC returns null for these when no discount / concession applies (per OSRTC flow doc step 3).
    -- Modelled as Maybe so the response decodes cleanly; default to 0 at use sites.
    intTotalDiscount :: Maybe Double,
    intConcessionAmount :: Maybe Double,
    intTotalFinalAmount :: Double,
    intTotalComponentFare :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OSRTCFareComponent = OSRTCFareComponent
  { intFareComponentID :: Int,
    strFareComponentName :: Text,
    decComponentWiseTotalFare :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- InsertTicketBooking
data OSRTCCategoryWiseBookingReq = OSRTCCategoryWiseBookingReq
  { intSrNo :: Int,
    intSeatTypeID :: OSRTCEnums.OSRTCSeatType,
    strSelectedSeatCode :: Text,
    strSelectedSeatNo :: Text,
    intBearthTypeID :: OSRTCEnums.OSRTCBerthType, -- intentional spelling mistake
    intTicketCategoryID :: OSRTCEnums.OSRTCTicketCategory,
    strPassengerName :: Text,
    intAge :: Int,
    strGender :: OSRTCEnums.OSRTCGender,
    strMobileNo :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance Show OSRTCCategoryWiseBookingReq where
  showsPrec _ r = showString $ "OSRTCCategoryWiseBookingReq {intSrNo = " <> T.unpack (show r.intSrNo) <> ", strSelectedSeatCode = " <> T.unpack r.strSelectedSeatCode <> ", passenger = <redacted>}"

data OSRTCInsertBookingReq = OSRTCInsertBookingReq
  { intServiceTripDepartureID :: Int,
    intSourcePlaceID :: Int,
    intDestinationPlaceID :: Int,
    strPassengerName :: Text,
    strEmail :: Text,
    strMobileNo :: Text,
    intTotalPaidAmount :: Double,
    intPlatformID :: Int,
    intPaymentModeID :: Int,
    strCategoryWiseJSON :: [OSRTCCategoryWiseBookingReq]
  }
  deriving (Generic, FromJSON, ToJSON)

instance Show OSRTCInsertBookingReq where
  showsPrec _ r = showString $ "OSRTCInsertBookingReq {intServiceTripDepartureID = " <> T.unpack (show r.intServiceTripDepartureID) <> ", intSourcePlaceID = " <> T.unpack (show r.intSourcePlaceID) <> ", intDestinationPlaceID = " <> T.unpack (show r.intDestinationPlaceID) <> ", passengers = <redacted>}"

type OSRTCInsertBookingResList = [OSRTCInsertBookingRes]

data OSRTCInsertBookingRes = OSRTCInsertBookingRes
  { intTicketBookingID :: Int,
    strPNRNo :: Text,
    strOrderNo :: Text,
    strPaymentObjRefNo :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- UpdateTicketBookingResponse (data is a single object, not an array)
data OSRTCUpdateBookingReq = OSRTCUpdateBookingReq
  { strPaymentObjRefNo :: Text,
    intPGPaymentStatusID :: Int,
    strStatusCode :: OSRTCEnums.OSRTCPGPaymentStatus,
    strBankRefNo :: Text,
    strPGTrackingRefNo :: Text,
    intAmount :: Double,
    strPaymentMode :: Text,
    strCardNumber :: Text,
    strBankStatus :: OSRTCEnums.OSRTCPGPaymentStatus,
    strPGType :: Text,
    strFullResponse :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance Show OSRTCUpdateBookingReq where
  showsPrec _ r = showString $ "OSRTCUpdateBookingReq {strPaymentObjRefNo = " <> T.unpack r.strPaymentObjRefNo <> ", strStatusCode = " <> T.unpack (show r.strStatusCode) <> ", payment = <redacted>}"

data OSRTCUpdateBookingRes = OSRTCUpdateBookingRes
  { strPaymentObjRefNo :: Text,
    strPNRNo :: Text,
    strQRCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance Show OSRTCUpdateBookingRes where
  showsPrec _ r = showString $ "OSRTCUpdateBookingRes {strPaymentObjRefNo = " <> T.unpack r.strPaymentObjRefNo <> ", strPNRNo = " <> T.unpack r.strPNRNo <> ", strQRCode = <redacted>}"

-- GetRefundAmount
data OSRTCGetRefundReq = OSRTCGetRefundReq
  { strSeatCodes :: Text,
    strPNRNo :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OSRTCGetRefundResList = [OSRTCGetRefundRes]

data OSRTCGetRefundRes = OSRTCGetRefundRes
  { intRefundAmount :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- InsertTicketCancel
data OSRTCCancelReq = OSRTCCancelReq
  { strPNRNo :: Text,
    strSeatCodes :: Text,
    strRefundRemarks :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OSRTCCancelResList = [OSRTCCancelRes]

data OSRTCCancelRes = OSRTCCancelRes
  { strPNRNo :: Text,
    intRefundAmount :: Double,
    strSeatCodes :: Text,
    strPaymentObjRefNo :: Text,
    intPGPaymentStatusID :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- GetTrackingData (live vehicle location; response shape varies — pass through as JSON)
data OSRTCGetTrackingReq = OSRTCGetTrackingReq
  { strPNRNo :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- MasterServiceList (no request payload; response shape unknown — pass through as JSON)
data OSRTCGetServiceListReq = OSRTCGetServiceListReq
  deriving (Show)

instance ToJSON OSRTCGetServiceListReq where
  toJSON _ = object []

instance FromJSON OSRTCGetServiceListReq where
  parseJSON _ = pure OSRTCGetServiceListReq

-- GetTripCrewList
data OSRTCGetTripCrewListReq = OSRTCGetTripCrewListReq
  { intServiceTripDepartureID :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)
