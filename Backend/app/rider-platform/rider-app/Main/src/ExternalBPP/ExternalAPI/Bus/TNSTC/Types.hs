{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Types
  ( setcNamespace,
    parseTnstcTimestamp,
    childText,
    nonEmptyText,
    ServiceIdParts (..),
    TnstcServiceVO (..),
    parseServiceIdCsv,
    parseAvailableServices,
    TnstcSeatSets (..),
    parseSeatSets,
    TnstcPickupPoint (..),
    TnstcConcessionType (..),
    TnstcBlockResult (..),
    TnstcFareResult (..),
    parsePickupPoints,
    parsePlaces,
    TnstcPlace (..),
    parseConcessionTypes,
    parseBlockResult,
    parseBookingResult,
    parseLookupValues,
    TnstcLookupValue (..),
    TnstcBookingResult (..),
    parseFareResult,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Text.XML.Cursor
import Prelude

setcNamespace :: Text
setcNamespace = "com.setc.otrws.service.bo"

childText :: Cursor -> Text -> Text
childText row name = T.concat (row $/ laxElement name &/ content)

nonEmptyText :: Text -> Maybe Text
nonEmptyText t = let s = T.strip t in if T.null s then Nothing else Just s

data ServiceIdParts = ServiceIdParts
  { sipServiceId :: Text,
    sipClassId :: Text,
    sipClassDesc :: Text,
    sipLayoutId :: Text,
    sipAvailableSeats :: Maybe Int,
    sipJourneyHours :: Text,
    sipDepartureTime :: Text,
    sipRouteNo :: Text,
    sipCorpCode :: Text,
    sipStatus :: Text
  }
  deriving (Show, Eq)

data TnstcServiceVO = TnstcServiceVO
  { svcParts :: ServiceIdParts,
    svcServiceClass :: Text,
    svcOrigin :: Text,
    svcDestination :: Text,
    svcAdultFare :: Maybe Double,
    svcChildFare :: Maybe Double,
    svcAdultSlpFare :: Maybe Double,
    svcChildSlpFare :: Maybe Double,
    svcArrivalTime :: Text,
    svcArrivalDate :: Text,
    svcDistance :: Maybe Double,
    svcRouteNo :: Text,
    svcTripCode :: Text,
    svcViaPlaces :: Maybe Text,
    svcPlatformNumber :: Maybe Text,
    svcStopBookingTime :: Maybe Text
  }
  deriving (Show, Eq)

parseServiceIdCsv :: Text -> Maybe ServiceIdParts
parseServiceIdCsv raw =
  case map T.strip (T.splitOn "," raw) of
    (sid : cls : desc : lay : seats : hrs : dep : route : corp : st : _)
      | not (T.null sid) ->
        Just
          ServiceIdParts
            { sipServiceId = sid,
              sipClassId = cls,
              sipClassDesc = desc,
              sipLayoutId = lay,
              sipAvailableSeats = readMaybe (T.unpack seats),
              sipJourneyHours = hrs,
              sipDepartureTime = dep,
              sipRouteNo = route,
              sipCorpCode = corp,
              sipStatus = st
            }
    _ -> Nothing

parseAvailableServices :: Cursor -> [TnstcServiceVO]
parseAvailableServices cur =
  mapMaybe toService (cur $// laxElement "GetAvailableServiceDetails")
  where
    toService row = do
      parts <- parseServiceIdCsv (childText row "serviceID")
      pure
        TnstcServiceVO
          { svcParts = parts,
            svcServiceClass = childText row "serviceClass",
            svcOrigin = childText row "origin",
            svcDestination = childText row "destination",
            svcAdultFare = readMaybe . T.unpack =<< nonEmptyText (childText row "adultFare"),
            svcChildFare = readMaybe . T.unpack =<< nonEmptyText (childText row "childFare"),
            svcAdultSlpFare = readMaybe . T.unpack =<< nonEmptyText (childText row "adultSLPFare"),
            svcChildSlpFare = readMaybe . T.unpack =<< nonEmptyText (childText row "childSLPFare"),
            svcArrivalTime = childText row "arrivalTime",
            svcArrivalDate = childText row "arrivalDate",
            svcDistance = readMaybe . T.unpack =<< nonEmptyText (childText row "distance"),
            svcRouteNo = childText row "routeNo",
            svcTripCode = childText row "tripCode",
            svcViaPlaces = nonEmptyText (childText row "viaPlaces"),
            svcPlatformNumber = nonEmptyText (childText row "platformNumber"),
            svcStopBookingTime = nonEmptyText (childText row "stopBookingTime")
          }

data TnstcSeatSets = TnstcSeatSets
  { tssSet0 :: [Text],
    tssSet1 :: [Text],
    tssSet2 :: [Text],
    tssSet3 :: [Text],
    tssSet4 :: [Text],
    tssSet5 :: [Text]
  }
  deriving (Show, Eq)

parseSeatSets :: Cursor -> TnstcSeatSets
parseSeatSets cur =
  let sets = map csv (cur $// laxElement "GetServiceSeatDetails")
      csv c = filter (not . T.null) (map T.strip (T.splitOn "," (T.concat (c $// content))))
      at n = case drop n sets of
        (x : _) -> x
        [] -> []
   in TnstcSeatSets (at 0) (at 1) (at 2) (at 3) (at 4) (at 5)

data TnstcPickupPoint = TnstcPickupPoint
  { tppPlaceId :: Text,
    tppName :: Text,
    tppTime :: Maybe Text,
    tppPlatformNo :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TnstcConcessionType = TnstcConcessionType
  { tctConcessionId :: Text,
    tctConcessionDesc :: Text,
    tctCategoryLookupId :: Maybe Text
  }
  deriving (Show, Eq)

newtype TnstcBlockResult = TnstcBlockResult
  { tbrSeatBlockIds :: [Text]
  }
  deriving (Show, Eq)

data TnstcFareResult = TnstcFareResult
  { tfrTotalFare :: Maybe Double,
    tfrBasicFare :: Maybe Double,
    tfrStatus :: Maybe Text,
    tfrErrorMessage :: Maybe Text,
    tfrWsRefNo :: Maybe Text,
    tfrAdultFare :: Maybe Double,
    tfrChildFare :: Maybe Double,
    tfrComponents :: [(Text, Double)]
  }
  deriving (Show, Eq)

-- | Every element in the response with its text, for diagnosing a parse that yields
-- nothing. These payloads are parsed against field names we inferred, so a silent empty
-- result is far more likely to be a name mismatch than an genuinely empty response.
data TnstcPlace = TnstcPlace
  { tpPlaceId :: Text,
    tpPlaceCode :: Text,
    tpStateCode :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | GetAddressPlaceList rows: <placeList> with placeID / placeCode / stateCode / placeName.
-- The state code is what tells intrastate (TN) travel from interstate.
parsePlaces :: Cursor -> [TnstcPlace]
parsePlaces cur = mapMaybe toPlace (cur $// laxElement "placeList")
  where
    toPlace row = do
      pid <- nonEmptyText (childText row "placeID")
      pure
        TnstcPlace
          { tpPlaceId = pid,
            tpPlaceCode = childText row "placeCode",
            tpStateCode = childText row "stateCode"
          }

parsePickupPoints :: Cursor -> [TnstcPickupPoint]
parsePickupPoints cur = mapMaybe toPoint (cur $// laxElement "GetAllServicePickupPointsByServiceID")
  where
    toPoint row = do
      raw <- nonEmptyText (childText row "pickupPointID")
      placeId <- case map T.strip (T.splitOn "," raw) of
        (pid : _) | not (T.null pid) -> Just pid
        _ -> Nothing
      pure
        TnstcPickupPoint
          { tppPlaceId = placeId,
            tppName = childText row "pickupPointName",
            tppTime = nonEmptyText (childText row "pickupTime"),
            tppPlatformNo = nonEmptyText (childText row "platformNo")
          }

parseConcessionTypes :: Cursor -> [TnstcConcessionType]
parseConcessionTypes cur = mapMaybe toC (cur $// laxElement "GetAllConcessionTypesByServiceID")
  where
    toC row = do
      cid <- nonEmptyText (childText row "concession")
      pure
        TnstcConcessionType
          { tctConcessionId = cid,
            tctConcessionDesc = childText row "concessionDesc",
            tctCategoryLookupId = nonEmptyText (childText row "categoryLookupID")
          }

parseBlockResult :: Cursor -> TnstcBlockResult
parseBlockResult cur =
  TnstcBlockResult
    { tbrSeatBlockIds = mapMaybe (nonEmptyText . T.concat . ($/ content)) (cur $// laxElement "seatBlockIds")
    }

-- | A row from GetActivelookUpValues. For IDPROOF/ONLINE_BOOKING these are the 9 ID types a
-- rider may present at boarding. Rows arrive wrapped in <timeList>, which is TNSTC's generic
-- list element rather than anything time-related.
data TnstcLookupValue = TnstcLookupValue
  { tlvId :: Text,
    tlvValue :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

parseLookupValues :: Cursor -> [TnstcLookupValue]
parseLookupValues cur =
  mapMaybe mk (cur $// laxElement "timeList")
  where
    txt n c = listToMaybe (mapMaybe (nonEmptyText . T.concat . ($/ content)) (c $// laxElement n))
    mk c = TnstcLookupValue <$> txt "lookupId" c <*> txt "lookupValue" c

data TnstcBookingResult = TnstcBookingResult
  { tbkPnrNumber :: Maybe Text,
    tbkPnrMasterId :: Maybe Text,
    tbkSeatIds :: Maybe Text,
    tbkTicketNumber :: Maybe Text,
    tbkStatus :: Maybe Text
  }
  deriving (Show, Eq)

-- | ConfirmAdvSeatBooking echoes the request and adds pnrNumber / pnrMasterID / seatIDs.
-- Failures arrive as SOAP faults, not as fields, so there is no errorMessage to read.
parseBookingResult :: Cursor -> TnstcBookingResult
parseBookingResult cur =
  TnstcBookingResult
    { tbkPnrNumber = txt "pnrNumber",
      tbkPnrMasterId = txt "pnrMasterID",
      tbkSeatIds = txt "seatIDs",
      tbkTicketNumber = txt "ticketNumber",
      tbkStatus = txt "status"
    }
  where
    txt n = listToMaybe (mapMaybe (nonEmptyText . T.concat . ($/ content)) (cur $// laxElement n))

fareComponentNames :: [Text]
fareComponentNames =
  [ "basicFare",
    "reservationFee",
    "serviceFee",
    "tollFee",
    "bridgeFee",
    "userFee",
    "infraStructureFee",
    "accidentReliefFund",
    "entryFee",
    "otherLevies",
    "weekdayConcession",
    "discounts"
  ]

parseFareResult :: Cursor -> TnstcFareResult
parseFareResult cur =
  TnstcFareResult
    { tfrTotalFare = num "totalFare",
      tfrBasicFare = num "basicFare",
      tfrStatus = txt "status",
      tfrErrorMessage = txt "errorMessage",
      tfrWsRefNo = txt "WSRefNo",
      tfrAdultFare = num "adultFare",
      tfrChildFare = num "childFare",
      tfrComponents = mapMaybe (\n -> (n,) <$> num n) fareComponentNames
    }
  where
    -- TNSTC repeats some elements once per seat, so take the first rather than
    -- concatenating every match into one unparseable string.
    txt n = listToMaybe (mapMaybe (nonEmptyText . T.concat . ($/ content)) (cur $// laxElement n))
    num n = readMaybe . T.unpack =<< txt n

parseTnstcTimestamp :: Text -> Maybe UTCTime
parseTnstcTimestamp raw =
  let istOffset = 19800 :: NominalDiffTime
   in addUTCTime (negate istOffset)
        <$> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (T.unpack (T.strip raw))
