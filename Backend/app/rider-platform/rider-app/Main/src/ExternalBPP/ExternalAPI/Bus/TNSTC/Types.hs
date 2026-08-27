{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Types
  ( setcNamespace,
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
    parseConcessionTypes,
    parseBlockResult,
    parseFareResult,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
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
  deriving (Show, Eq)

data TnstcConcessionType = TnstcConcessionType
  { tctConcessionId :: Text,
    tctConcessionDesc :: Text,
    tctCategoryLookupId :: Maybe Text
  }
  deriving (Show, Eq)

data TnstcBlockResult = TnstcBlockResult
  { tbrSeatBlockIds :: Maybe Text,
    tbrBlockedReferenceNo :: Maybe Text,
    tbrBlockingKeyNo :: Maybe Text,
    tbrStatus :: Maybe Text,
    tbrErrorMessage :: Maybe Text
  }
  deriving (Show, Eq)

data TnstcFareResult = TnstcFareResult
  { tfrTotalFare :: Maybe Double,
    tfrBasicFare :: Maybe Double,
    tfrStatus :: Maybe Text,
    tfrErrorMessage :: Maybe Text,
    tfrComponents :: [(Text, Double)]
  }
  deriving (Show, Eq)

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
    { tbrSeatBlockIds = firstOf "seatBlockIds",
      tbrBlockedReferenceNo = firstOf "blockedReferenceNo",
      tbrBlockingKeyNo = firstOf "blockingKeyNo",
      tbrStatus = firstOf "status",
      tbrErrorMessage = firstOf "errorMessage"
    }
  where
    firstOf n = nonEmptyText (T.concat (cur $// laxElement n &/ content))

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
    "otherConcession"
  ]

parseFareResult :: Cursor -> TnstcFareResult
parseFareResult cur =
  TnstcFareResult
    { tfrTotalFare = num "totalFare",
      tfrBasicFare = num "basicFare",
      tfrStatus = txt "status",
      tfrErrorMessage = txt "errorMessage",
      tfrComponents = mapMaybe (\n -> (n,) <$> num n) fareComponentNames
    }
  where
    txt n = nonEmptyText (T.concat (cur $// laxElement n &/ content))
    num n = readMaybe . T.unpack =<< txt n
