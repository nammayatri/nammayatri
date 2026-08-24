{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Types
  ( setcNamespace,
    childText,
    nonEmptyText,
    ServiceIdParts (..),
    TnstcServiceVO (..),
    parseServiceIdCsv,
    parseAvailableServices,
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
            svcArrivalTime = childText row "arrivalTime",
            svcArrivalDate = childText row "arrivalDate",
            svcDistance = readMaybe . T.unpack =<< nonEmptyText (childText row "distance"),
            svcRouteNo = childText row "routeNo",
            svcTripCode = childText row "tripCode",
            svcViaPlaces = nonEmptyText (childText row "viaPlaces"),
            svcPlatformNumber = nonEmptyText (childText row "platformNumber"),
            svcStopBookingTime = nonEmptyText (childText row "stopBookingTime")
          }
